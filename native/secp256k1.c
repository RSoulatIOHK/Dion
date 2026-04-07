/*
 * Secp256k1 ECDSA and Schnorr (BIP-340) Signature Verification FFI for Dion
 *
 * This implements Plutus builtins #50 (verifyEcdsaSecp256k1Signature) and
 * #51 (verifySchnorrSecp256k1Signature) using a reference implementation of
 * the secp256k1 elliptic curve.
 *
 * The implementation uses uint64_t[4] limb representation for 256-bit field
 * arithmetic with proper carry propagation. It is correct but not constant-time
 * (appropriate for script validation, not key generation).
 *
 * References:
 * - SEC 2: Recommended Elliptic Curve Domain Parameters (secp256k1)
 * - BIP-340: Schnorr Signatures for secp256k1
 * - Cardano Plutus Core Specification: Builtin Functions
 */

#include <lean/lean.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

/* ============================================================
 * 256-bit Unsigned Integer Arithmetic (4 x uint64_t limbs)
 * Limb order: u256[0] = least significant, u256[3] = most significant
 * ============================================================ */

typedef uint64_t u256[4];

static void u256_zero(u256 r) {
    r[0] = r[1] = r[2] = r[3] = 0;
}

static void u256_copy(u256 r, const u256 a) {
    r[0] = a[0]; r[1] = a[1]; r[2] = a[2]; r[3] = a[3];
}

static int u256_is_zero(const u256 a) {
    return (a[0] | a[1] | a[2] | a[3]) == 0;
}

/* Compare: returns -1, 0, or 1 */
static int u256_cmp(const u256 a, const u256 b) {
    for (int i = 3; i >= 0; i--) {
        if (a[i] < b[i]) return -1;
        if (a[i] > b[i]) return 1;
    }
    return 0;
}

/* r = a + b, returns carry */
static uint64_t u256_add(u256 r, const u256 a, const u256 b) {
    __uint128_t carry = 0;
    for (int i = 0; i < 4; i++) {
        carry += (__uint128_t)a[i] + b[i];
        r[i] = (uint64_t)carry;
        carry >>= 64;
    }
    return (uint64_t)carry;
}

/* r = a - b, returns borrow (0 or 1) */
static uint64_t u256_sub(u256 r, const u256 a, const u256 b) {
    __uint128_t borrow = 0;
    for (int i = 0; i < 4; i++) {
        __uint128_t diff = (__uint128_t)a[i] - b[i] - borrow;
        r[i] = (uint64_t)diff;
        borrow = (diff >> 127) & 1; /* borrow if negative */
    }
    return (uint64_t)borrow;
}

/* r = a * b (512-bit result in r_hi:r_lo) */
static void u256_mul(u256 r_lo, u256 r_hi, const u256 a, const u256 b) {
    uint64_t t[8] = {0};
    for (int i = 0; i < 4; i++) {
        __uint128_t carry = 0;
        for (int j = 0; j < 4; j++) {
            __uint128_t prod = (__uint128_t)a[i] * b[j] + t[i + j] + carry;
            t[i + j] = (uint64_t)prod;
            carry = prod >> 64;
        }
        t[i + 4] = (uint64_t)carry;
    }
    r_lo[0] = t[0]; r_lo[1] = t[1]; r_lo[2] = t[2]; r_lo[3] = t[3];
    r_hi[0] = t[4]; r_hi[1] = t[5]; r_hi[2] = t[6]; r_hi[3] = t[7];
}

/* Load from big-endian bytes */
static void u256_from_be(u256 r, const uint8_t *bytes, size_t len) {
    u256_zero(r);
    for (size_t i = 0; i < len && i < 32; i++) {
        int limb = (int)((31 - i) / 8);
        int shift = (int)(((31 - i) % 8) * 8);
        r[limb] |= (uint64_t)bytes[i] << shift;
    }
}

/* Store to big-endian bytes (32 bytes) */
static void u256_to_be(uint8_t *bytes, const u256 a) {
    for (int i = 0; i < 32; i++) {
        int limb = (31 - i) / 8;
        int shift = ((31 - i) % 8) * 8;
        bytes[i] = (uint8_t)(a[limb] >> shift);
    }
}

/* ============================================================
 * Modular Arithmetic over secp256k1 field prime p and order n
 * ============================================================ */

/* secp256k1 field prime: p = 2^256 - 2^32 - 977 */
static const u256 SECP256K1_P = {
    0xFFFFFFFEFFFFFC2FULL, 0xFFFFFFFFFFFFFFFFULL,
    0xFFFFFFFFFFFFFFFFULL, 0xFFFFFFFFFFFFFFFFULL
};

/* secp256k1 group order */
static const u256 SECP256K1_N = {
    0xBFD25E8CD0364141ULL, 0xBAAEDCE6AF48A03BULL,
    0xFFFFFFFFFFFFFFFEULL, 0xFFFFFFFFFFFFFFFFULL
};

/* r = a mod m, given a < 2*m */
static void mod_reduce(u256 r, const u256 a, const u256 m) {
    u256 t;
    uint64_t borrow = u256_sub(t, a, m);
    if (borrow) {
        u256_copy(r, a);
    } else {
        u256_copy(r, t);
    }
}

/* r = (a + b) mod m */
static void mod_add(u256 r, const u256 a, const u256 b, const u256 m) {
    uint64_t carry = u256_add(r, a, b);
    if (carry || u256_cmp(r, m) >= 0) {
        u256_sub(r, r, m);
    }
}

/* r = (a - b) mod m */
static void mod_sub(u256 r, const u256 a, const u256 b, const u256 m) {
    uint64_t borrow = u256_sub(r, a, b);
    if (borrow) {
        u256_add(r, r, m);
    }
}

/* r = (a * b) mod m using Barrett-like reduction:
   Compute a*b as 512-bit, then reduce mod m via trial subtraction. */
static void mod_mul(u256 r, const u256 a, const u256 b, const u256 m) {
    u256 lo, hi;
    u256_mul(lo, hi, a, b);

    /* Reduce 512-bit result mod m by repeated subtraction.
       For efficiency, we do shift-and-subtract reduction. */
    /* Simple approach: use the fact that for secp256k1_p,
       2^256 ≡ 2^32 + 977 (mod p), and for n we just do general reduction. */

    /* General modular reduction using schoolbook division-like approach */
    /* We'll build the result by processing hi:lo */

    /* For correctness, we use a simple but correct approach:
       Process each bit of hi, shifting into lo, and reduce. */
    u256 acc;
    u256_copy(acc, lo);

    /* Reduce acc mod m first */
    while (u256_cmp(acc, m) >= 0) {
        u256_sub(acc, acc, m);
    }

    /* Now process hi: for each bit position from 255 down to 0,
       acc = (acc + hi_bit * 2^(256+bitpos)) mod m
       But this is slow. Instead, use the identity:
       hi * 2^256 mod m. We compute 2^256 mod m, then multiply. */

    /* Compute 2^256 mod m */
    u256 pow256_mod_m;
    u256_zero(pow256_mod_m);
    /* 2^256 mod p = 0x1000003D1 for secp256k1 p */
    /* 2^256 mod n: we compute it */
    {
        /* 2^256 = m + (2^256 - m) = m + r, so 2^256 mod m = 2^256 - m */
        u256 one = {1, 0, 0, 0};
        /* Actually, since m < 2^256, 2^256 mod m = 2^256 - m if 2^256 > m,
           but 2^256 doesn't fit in u256. So we note:
           2^256 - 1 is the max u256. 2^256 mod m = (2^256 - m).
           Since m < 2^256, 2^256 - m > 0 and fits in u256. */
        u256 max_val = {0xFFFFFFFFFFFFFFFFULL, 0xFFFFFFFFFFFFFFFFULL,
                        0xFFFFFFFFFFFFFFFFULL, 0xFFFFFFFFFFFFFFFFULL};
        u256_sub(pow256_mod_m, max_val, m);
        u256_add(pow256_mod_m, pow256_mod_m, one);
        /* Now pow256_mod_m = 2^256 - m = 2^256 mod m */
        while (u256_cmp(pow256_mod_m, m) >= 0) {
            u256_sub(pow256_mod_m, pow256_mod_m, m);
        }
    }

    /* Now compute hi * pow256_mod_m mod m, add to acc */
    /* But hi * pow256_mod_m is potentially 512 bits again.
       We need to do this iteratively. Actually for secp256k1:
       - For p: pow256_mod_p = 0x1000003D1, which is small, so hi * small fits
       - For n: pow256_mod_n is also relatively small
       Let's just do schoolbook: multiply hi by pow256_mod_m,
       get 512 bits, then reduce again. Since pow256_mod_m is small
       for secp256k1, the hi part of this product will be zero or small. */

    u256 prod_lo, prod_hi;
    u256_mul(prod_lo, prod_hi, hi, pow256_mod_m);

    /* prod_hi should be zero or very small for secp256k1 parameters */
    /* Reduce prod_lo mod m */
    while (u256_cmp(prod_lo, m) >= 0) {
        u256_sub(prod_lo, prod_lo, m);
    }

    /* If prod_hi is nonzero, process it recursively (should not happen for secp256k1) */
    if (!u256_is_zero(prod_hi)) {
        u256 inner_lo, inner_hi;
        u256_mul(inner_lo, inner_hi, prod_hi, pow256_mod_m);
        while (u256_cmp(inner_lo, m) >= 0) {
            u256_sub(inner_lo, inner_lo, m);
        }
        mod_add(prod_lo, prod_lo, inner_lo, m);
    }

    mod_add(r, acc, prod_lo, m);
}

/* r = a^2 mod m */
static void mod_sqr(u256 r, const u256 a, const u256 m) {
    mod_mul(r, a, a, m);
}

/* r = a^exp mod m (binary method, exp is u256) */
static void mod_exp(u256 r, const u256 base, const u256 exp, const u256 m) {
    u256 b, e;
    u256_copy(b, base);
    u256_copy(e, exp);
    r[0] = 1; r[1] = 0; r[2] = 0; r[3] = 0;

    for (int i = 0; i < 256; i++) {
        if (e[0] & 1) {
            mod_mul(r, r, b, m);
        }
        mod_sqr(b, b, m);
        /* Shift e right by 1 */
        e[0] = (e[0] >> 1) | (e[1] << 63);
        e[1] = (e[1] >> 1) | (e[2] << 63);
        e[2] = (e[2] >> 1) | (e[3] << 63);
        e[3] >>= 1;
    }
}

/* r = a^(-1) mod m using Fermat's little theorem: a^(m-2) mod m */
static void mod_inv(u256 r, const u256 a, const u256 m) {
    u256 exp;
    u256 two = {2, 0, 0, 0};
    u256_sub(exp, m, two);
    mod_exp(r, a, exp, m);
}

/* Compute modular square root: r = a^((p+1)/4) mod p
   Works because p ≡ 3 (mod 4) for secp256k1 */
static void mod_sqrt(u256 r, const u256 a, const u256 p) {
    u256 exp;
    u256 one = {1, 0, 0, 0};
    u256_copy(exp, p);
    u256_add(exp, exp, one);
    /* exp = p + 1 */
    /* Shift right by 2: exp = (p+1)/4 */
    exp[0] = (exp[0] >> 2) | (exp[1] << 62);
    exp[1] = (exp[1] >> 2) | (exp[2] << 62);
    exp[2] = (exp[2] >> 2) | (exp[3] << 62);
    exp[3] >>= 2;
    mod_exp(r, a, exp, p);
}

/* ============================================================
 * secp256k1 Elliptic Curve Point Operations
 * Using Jacobian coordinates: (X, Y, Z) represents (X/Z^2, Y/Z^3)
 * ============================================================ */

typedef struct {
    u256 x, y, z;
} point_j;

/* Generator point G (affine coordinates) */
static const u256 GX = {
    0x59F2815B16F81798ULL, 0x029BFCDB2DCE28D9ULL,
    0x55A06295CE870B07ULL, 0x79BE667EF9DCBBACULL
};
static const u256 GY = {
    0x9C47D08FFB10D4B8ULL, 0xFD17B448A6855419ULL,
    0x5DA4FBFC0E1108A8ULL, 0x483ADA7726A3C465ULL
};

/* Set point to infinity (Z = 0) */
static void point_j_inf(point_j *p) {
    u256_zero(p->x);
    u256_zero(p->y);
    u256_zero(p->z);
    p->x[0] = 0; /* Infinity is represented as Z=0 */
}

static int point_j_is_inf(const point_j *p) {
    return u256_is_zero(p->z);
}

/* Convert affine (x, y) to Jacobian */
static void point_j_from_affine(point_j *p, const u256 x, const u256 y) {
    u256_copy(p->x, x);
    u256_copy(p->y, y);
    u256_zero(p->z);
    p->z[0] = 1;
}

/* Convert Jacobian to affine (x, y). Returns 0 if point at infinity. */
static int point_j_to_affine(u256 x, u256 y, const point_j *p) {
    if (point_j_is_inf(p)) return 0;
    u256 z_inv, z_inv2, z_inv3;
    mod_inv(z_inv, p->z, SECP256K1_P);
    mod_sqr(z_inv2, z_inv, SECP256K1_P);
    mod_mul(z_inv3, z_inv2, z_inv, SECP256K1_P);
    mod_mul(x, p->x, z_inv2, SECP256K1_P);
    mod_mul(y, p->y, z_inv3, SECP256K1_P);
    return 1;
}

/* Point doubling in Jacobian coordinates
   Using the formulas from "Guide to ECC" (Hankerson, Menezes, Vanstone)
   For a = 0 (secp256k1): */
static void point_j_double(point_j *r, const point_j *p) {
    if (point_j_is_inf(p) || u256_is_zero(p->y)) {
        point_j_inf(r);
        return;
    }

    u256 A, B, C, D, E, F;

    /* A = Y1^2 */
    mod_sqr(A, p->y, SECP256K1_P);
    /* B = 4*X1*A */
    mod_mul(B, p->x, A, SECP256K1_P);
    mod_add(B, B, B, SECP256K1_P);
    mod_add(B, B, B, SECP256K1_P);
    /* C = 8*A^2 */
    mod_sqr(C, A, SECP256K1_P);
    mod_add(C, C, C, SECP256K1_P);
    mod_add(C, C, C, SECP256K1_P);
    mod_add(C, C, C, SECP256K1_P);
    /* D = 3*X1^2 (since a=0 for secp256k1) */
    mod_sqr(D, p->x, SECP256K1_P);
    u256_copy(E, D);
    mod_add(D, D, E, SECP256K1_P);
    mod_add(D, D, E, SECP256K1_P);

    /* X3 = D^2 - 2*B */
    mod_sqr(F, D, SECP256K1_P);
    mod_sub(r->x, F, B, SECP256K1_P);
    mod_sub(r->x, r->x, B, SECP256K1_P);

    /* Y3 = D*(B - X3) - C */
    mod_sub(E, B, r->x, SECP256K1_P);
    mod_mul(r->y, D, E, SECP256K1_P);
    mod_sub(r->y, r->y, C, SECP256K1_P);

    /* Z3 = 2*Y1*Z1 */
    mod_mul(r->z, p->y, p->z, SECP256K1_P);
    mod_add(r->z, r->z, r->z, SECP256K1_P);
}

/* Point addition in Jacobian coordinates (mixed: Q is affine, i.e., Q.z=1) */
static void point_j_add_affine(point_j *r, const point_j *p, const u256 qx, const u256 qy) {
    if (point_j_is_inf(p)) {
        point_j_from_affine(r, qx, qy);
        return;
    }

    u256 Z1sq, U2, S2, H, HH, I, J, rr, V;

    /* U1 = X1, S1 = Y1 (since Z2 = 1 for affine Q) */
    /* Z1^2 */
    mod_sqr(Z1sq, p->z, SECP256K1_P);
    /* U2 = X2*Z1^2 */
    mod_mul(U2, qx, Z1sq, SECP256K1_P);
    /* S2 = Y2*Z1^3 */
    mod_mul(S2, Z1sq, p->z, SECP256K1_P);
    mod_mul(S2, qy, S2, SECP256K1_P);

    /* H = U2 - X1 */
    mod_sub(H, U2, p->x, SECP256K1_P);
    /* rr = S2 - Y1 */
    mod_sub(rr, S2, p->y, SECP256K1_P);

    if (u256_is_zero(H)) {
        if (u256_is_zero(rr)) {
            /* P == Q, do doubling */
            point_j_double(r, p);
            return;
        } else {
            /* P == -Q, result is infinity */
            point_j_inf(r);
            return;
        }
    }

    /* HH = H^2 */
    mod_sqr(HH, H, SECP256K1_P);
    /* I = 4*HH */
    mod_add(I, HH, HH, SECP256K1_P);
    mod_add(I, I, I, SECP256K1_P);
    /* J = H * I */
    mod_mul(J, H, I, SECP256K1_P);
    /* rr = 2*(S2 - Y1) */
    mod_add(rr, rr, rr, SECP256K1_P);
    /* V = X1 * I */
    mod_mul(V, p->x, I, SECP256K1_P);

    /* X3 = rr^2 - J - 2*V */
    mod_sqr(r->x, rr, SECP256K1_P);
    mod_sub(r->x, r->x, J, SECP256K1_P);
    mod_sub(r->x, r->x, V, SECP256K1_P);
    mod_sub(r->x, r->x, V, SECP256K1_P);

    /* Y3 = rr*(V - X3) - 2*Y1*J */
    mod_sub(V, V, r->x, SECP256K1_P);
    mod_mul(r->y, rr, V, SECP256K1_P);
    u256 tmp;
    mod_mul(tmp, p->y, J, SECP256K1_P);
    mod_add(tmp, tmp, tmp, SECP256K1_P);
    mod_sub(r->y, r->y, tmp, SECP256K1_P);

    /* Z3 = (Z1 + H)^2 - Z1^2 - HH */
    mod_add(r->z, p->z, H, SECP256K1_P);
    mod_sqr(r->z, r->z, SECP256K1_P);
    mod_sub(r->z, r->z, Z1sq, SECP256K1_P);
    mod_sub(r->z, r->z, HH, SECP256K1_P);
}

/* Full Jacobian addition (both points Jacobian) */
static void point_j_add(point_j *r, const point_j *p, const point_j *q) {
    if (point_j_is_inf(p)) { *r = *q; return; }
    if (point_j_is_inf(q)) { *r = *p; return; }

    u256 Z1sq, Z2sq, U1, U2, S1, S2, H, rr;

    mod_sqr(Z1sq, p->z, SECP256K1_P);
    mod_sqr(Z2sq, q->z, SECP256K1_P);
    mod_mul(U1, p->x, Z2sq, SECP256K1_P);
    mod_mul(U2, q->x, Z1sq, SECP256K1_P);

    u256 Z1cu, Z2cu;
    mod_mul(Z1cu, Z1sq, p->z, SECP256K1_P);
    mod_mul(Z2cu, Z2sq, q->z, SECP256K1_P);
    mod_mul(S1, p->y, Z2cu, SECP256K1_P);
    mod_mul(S2, q->y, Z1cu, SECP256K1_P);

    mod_sub(H, U2, U1, SECP256K1_P);
    mod_sub(rr, S2, S1, SECP256K1_P);

    if (u256_is_zero(H)) {
        if (u256_is_zero(rr)) {
            point_j_double(r, p);
            return;
        } else {
            point_j_inf(r);
            return;
        }
    }

    u256 HH, HHH, V;
    mod_sqr(HH, H, SECP256K1_P);
    mod_mul(HHH, HH, H, SECP256K1_P);
    mod_mul(V, U1, HH, SECP256K1_P);

    /* X3 = rr^2 - HHH - 2*V */
    mod_sqr(r->x, rr, SECP256K1_P);
    mod_sub(r->x, r->x, HHH, SECP256K1_P);
    mod_sub(r->x, r->x, V, SECP256K1_P);
    mod_sub(r->x, r->x, V, SECP256K1_P);

    /* Y3 = rr*(V - X3) - S1*HHH */
    u256 tmp;
    mod_sub(tmp, V, r->x, SECP256K1_P);
    mod_mul(r->y, rr, tmp, SECP256K1_P);
    mod_mul(tmp, S1, HHH, SECP256K1_P);
    mod_sub(r->y, r->y, tmp, SECP256K1_P);

    /* Z3 = Z1*Z2*H */
    mod_mul(r->z, p->z, q->z, SECP256K1_P);
    mod_mul(r->z, r->z, H, SECP256K1_P);
}

/* Scalar multiplication: r = k * G (double-and-add) */
static void scalar_mul_base(point_j *r, const u256 k) {
    point_j_inf(r);
    point_j G;
    point_j_from_affine(&G, GX, GY);

    /* Simple double-and-add from MSB */
    int started = 0;
    for (int i = 255; i >= 0; i--) {
        int limb = i / 64;
        int bit = i % 64;
        if (started) {
            point_j tmp;
            point_j_double(&tmp, r);
            *r = tmp;
        }
        if ((k[limb] >> bit) & 1) {
            if (!started) {
                *r = G;
                started = 1;
            } else {
                point_j tmp;
                point_j_add_affine(&tmp, r, GX, GY);
                *r = tmp;
            }
        }
    }
}

/* Scalar multiplication of arbitrary point: r = k * P */
static void scalar_mul_point(point_j *r, const u256 k, const point_j *P) {
    point_j_inf(r);
    point_j base = *P;

    int started = 0;
    for (int i = 255; i >= 0; i--) {
        int limb = i / 64;
        int bit = i % 64;
        if (started) {
            point_j tmp;
            point_j_double(&tmp, r);
            *r = tmp;
        }
        if ((k[limb] >> bit) & 1) {
            if (!started) {
                *r = base;
                started = 1;
            } else {
                point_j tmp;
                point_j_add(&tmp, r, &base);
                *r = tmp;
            }
        }
    }
    (void)base; /* suppress unused warning */
}

/* Simultaneous scalar mul: r = u1*G + u2*P (Shamir's trick) */
static void scalar_mul_double(point_j *r, const u256 u1, const u256 u2,
                               const u256 px, const u256 py) {
    /* Precompute: G, P, G+P */
    point_j G, P, GP;
    point_j_from_affine(&G, GX, GY);
    point_j_from_affine(&P, px, py);
    point_j_add(&GP, &G, &P);

    point_j_inf(r);
    int started = 0;

    for (int i = 255; i >= 0; i--) {
        int limb = i / 64;
        int bit = i % 64;
        int b1 = (u1[limb] >> bit) & 1;
        int b2 = (u2[limb] >> bit) & 1;

        if (started) {
            point_j tmp;
            point_j_double(&tmp, r);
            *r = tmp;
        }

        const point_j *to_add = NULL;
        if (b1 && b2) to_add = &GP;
        else if (b1) to_add = &G;
        else if (b2) to_add = &P;

        if (to_add) {
            if (!started) {
                *r = *to_add;
                started = 1;
            } else {
                point_j tmp;
                point_j_add(&tmp, r, to_add);
                *r = tmp;
            }
        }
    }
}

/* ============================================================
 * Decompress a secp256k1 compressed public key (33 bytes)
 * Returns 1 on success, 0 on failure
 * ============================================================ */

static int decompress_pubkey(u256 x, u256 y, const uint8_t *pk, size_t pk_len) {
    if (pk_len != 33) return 0;
    uint8_t prefix = pk[0];
    if (prefix != 0x02 && prefix != 0x03) return 0;

    u256_from_be(x, pk + 1, 32);

    /* Check x < p */
    if (u256_cmp(x, SECP256K1_P) >= 0) return 0;

    /* y^2 = x^3 + 7 (mod p) */
    u256 x2, x3, y2, seven;
    mod_sqr(x2, x, SECP256K1_P);
    mod_mul(x3, x2, x, SECP256K1_P);
    u256_zero(seven);
    seven[0] = 7;
    mod_add(y2, x3, seven, SECP256K1_P);

    /* y = sqrt(y^2) mod p, p ≡ 3 mod 4 so y = y2^((p+1)/4) */
    mod_sqrt(y, y2, SECP256K1_P);

    /* Verify: y^2 == y2 */
    u256 check;
    mod_sqr(check, y, SECP256K1_P);
    if (u256_cmp(check, y2) != 0) return 0;

    /* Choose correct y parity */
    int y_is_odd = y[0] & 1;
    int want_odd = (prefix == 0x03) ? 1 : 0;
    if (y_is_odd != want_odd) {
        mod_sub(y, SECP256K1_P, y, SECP256K1_P);
    }

    return 1;
}

/* Lift x-only public key (32 bytes) to a point with even y (BIP-340) */
static int lift_x(u256 x, u256 y, const uint8_t *pk32) {
    u256_from_be(x, pk32, 32);
    if (u256_cmp(x, SECP256K1_P) >= 0) return 0;

    u256 x2, x3, y2, seven;
    mod_sqr(x2, x, SECP256K1_P);
    mod_mul(x3, x2, x, SECP256K1_P);
    u256_zero(seven);
    seven[0] = 7;
    mod_add(y2, x3, seven, SECP256K1_P);

    mod_sqrt(y, y2, SECP256K1_P);

    u256 check;
    mod_sqr(check, y, SECP256K1_P);
    if (u256_cmp(check, y2) != 0) return 0;

    /* BIP-340: choose even y */
    if (y[0] & 1) {
        mod_sub(y, SECP256K1_P, y, SECP256K1_P);
    }

    return 1;
}

/* ============================================================
 * Minimal SHA-256 Implementation (for BIP-340 tagged hashing)
 * ============================================================ */

static const uint32_t sha256_k[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

typedef struct {
    uint32_t state[8];
    uint8_t buf[64];
    uint64_t total_len;
    size_t buf_len;
} sha256_ctx;

static uint32_t sha256_rotr(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }
static uint32_t sha256_ch(uint32_t x, uint32_t y, uint32_t z) { return (x & y) ^ (~x & z); }
static uint32_t sha256_maj(uint32_t x, uint32_t y, uint32_t z) { return (x & y) ^ (x & z) ^ (y & z); }
static uint32_t sha256_sigma0(uint32_t x) { return sha256_rotr(x, 2) ^ sha256_rotr(x, 13) ^ sha256_rotr(x, 22); }
static uint32_t sha256_sigma1(uint32_t x) { return sha256_rotr(x, 6) ^ sha256_rotr(x, 11) ^ sha256_rotr(x, 25); }
static uint32_t sha256_gamma0(uint32_t x) { return sha256_rotr(x, 7) ^ sha256_rotr(x, 18) ^ (x >> 3); }
static uint32_t sha256_gamma1(uint32_t x) { return sha256_rotr(x, 17) ^ sha256_rotr(x, 19) ^ (x >> 10); }

static void sha256_transform(sha256_ctx *ctx, const uint8_t *data) {
    uint32_t w[64];
    for (int i = 0; i < 16; i++) {
        w[i] = ((uint32_t)data[i*4] << 24) | ((uint32_t)data[i*4+1] << 16) |
                ((uint32_t)data[i*4+2] << 8) | data[i*4+3];
    }
    for (int i = 16; i < 64; i++) {
        w[i] = sha256_gamma1(w[i-2]) + w[i-7] + sha256_gamma0(w[i-15]) + w[i-16];
    }

    uint32_t a = ctx->state[0], b = ctx->state[1], c = ctx->state[2], d = ctx->state[3];
    uint32_t e = ctx->state[4], f = ctx->state[5], g = ctx->state[6], h = ctx->state[7];

    for (int i = 0; i < 64; i++) {
        uint32_t t1 = h + sha256_sigma1(e) + sha256_ch(e, f, g) + sha256_k[i] + w[i];
        uint32_t t2 = sha256_sigma0(a) + sha256_maj(a, b, c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    ctx->state[0] += a; ctx->state[1] += b; ctx->state[2] += c; ctx->state[3] += d;
    ctx->state[4] += e; ctx->state[5] += f; ctx->state[6] += g; ctx->state[7] += h;
}

static void sha256_init(sha256_ctx *ctx) {
    ctx->state[0] = 0x6a09e667; ctx->state[1] = 0xbb67ae85;
    ctx->state[2] = 0x3c6ef372; ctx->state[3] = 0xa54ff53a;
    ctx->state[4] = 0x510e527f; ctx->state[5] = 0x9b05688c;
    ctx->state[6] = 0x1f83d9ab; ctx->state[7] = 0x5be0cd19;
    ctx->total_len = 0;
    ctx->buf_len = 0;
}

static void sha256_update(sha256_ctx *ctx, const uint8_t *data, size_t len) {
    ctx->total_len += len;
    if (ctx->buf_len > 0) {
        size_t need = 64 - ctx->buf_len;
        if (len < need) {
            memcpy(ctx->buf + ctx->buf_len, data, len);
            ctx->buf_len += len;
            return;
        }
        memcpy(ctx->buf + ctx->buf_len, data, need);
        sha256_transform(ctx, ctx->buf);
        data += need;
        len -= need;
        ctx->buf_len = 0;
    }
    while (len >= 64) {
        sha256_transform(ctx, data);
        data += 64;
        len -= 64;
    }
    if (len > 0) {
        memcpy(ctx->buf, data, len);
        ctx->buf_len = len;
    }
}

static void sha256_final(sha256_ctx *ctx, uint8_t *digest) {
    uint64_t total_bits = ctx->total_len * 8;
    uint8_t pad = 0x80;
    sha256_update(ctx, &pad, 1);
    uint8_t zero = 0;
    while (ctx->buf_len != 56) {
        sha256_update(ctx, &zero, 1);
    }
    uint8_t len_be[8];
    for (int i = 7; i >= 0; i--) {
        len_be[i] = (uint8_t)(total_bits & 0xff);
        total_bits >>= 8;
    }
    sha256_update(ctx, len_be, 8);
    for (int i = 0; i < 8; i++) {
        digest[i*4]   = (uint8_t)(ctx->state[i] >> 24);
        digest[i*4+1] = (uint8_t)(ctx->state[i] >> 16);
        digest[i*4+2] = (uint8_t)(ctx->state[i] >> 8);
        digest[i*4+3] = (uint8_t)(ctx->state[i]);
    }
}

static void sha256_hash(uint8_t *digest, const uint8_t *data, size_t len) {
    sha256_ctx ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, data, len);
    sha256_final(&ctx, digest);
}

/* BIP-340 tagged hash: SHA256(SHA256(tag) || SHA256(tag) || msg) */
static void tagged_hash(uint8_t *out, const char *tag, const uint8_t *msg, size_t msg_len) {
    uint8_t tag_hash[32];
    sha256_hash(tag_hash, (const uint8_t *)tag, strlen(tag));

    sha256_ctx ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, tag_hash, 32);
    sha256_update(&ctx, tag_hash, 32);
    sha256_update(&ctx, msg, msg_len);
    sha256_final(&ctx, out);
}

/* ============================================================
 * ECDSA Verification (Plutus builtin #50)
 * ============================================================ */

static int secp256k1_ecdsa_verify_impl(const uint8_t *pk33, const uint8_t *msg32,
                                         const uint8_t *sig64) {
    u256 px, py;
    if (!decompress_pubkey(px, py, pk33, 33)) return 0;

    u256 r, s;
    u256_from_be(r, sig64, 32);
    u256_from_be(s, sig64 + 32, 32);

    /* Check r, s in [1, n-1] */
    if (u256_is_zero(r) || u256_cmp(r, SECP256K1_N) >= 0) return 0;
    if (u256_is_zero(s) || u256_cmp(s, SECP256K1_N) >= 0) return 0;

    /* z = message hash (already 32 bytes) */
    u256 z;
    u256_from_be(z, msg32, 32);

    /* s_inv = s^(-1) mod n */
    u256 s_inv;
    mod_inv(s_inv, s, SECP256K1_N);

    /* u1 = z * s_inv mod n */
    u256 u1;
    mod_mul(u1, z, s_inv, SECP256K1_N);

    /* u2 = r * s_inv mod n */
    u256 u2;
    mod_mul(u2, r, s_inv, SECP256K1_N);

    /* R = u1*G + u2*PK */
    point_j R;
    scalar_mul_double(&R, u1, u2, px, py);

    if (point_j_is_inf(&R)) return 0;

    /* Get affine x-coordinate */
    u256 rx, ry;
    if (!point_j_to_affine(rx, ry, &R)) return 0;

    /* Reduce rx mod n (in case rx >= n, though very unlikely for secp256k1) */
    while (u256_cmp(rx, SECP256K1_N) >= 0) {
        u256_sub(rx, rx, SECP256K1_N);
    }

    /* Check r == rx mod n */
    return u256_cmp(r, rx) == 0;
}

/* ============================================================
 * Schnorr Verification (BIP-340, Plutus builtin #51)
 * ============================================================ */

static int secp256k1_schnorr_verify_impl(const uint8_t *pk32, const uint8_t *msg,
                                           size_t msg_len, const uint8_t *sig64) {
    /* Parse signature: R.x (32 bytes) || s (32 bytes) */
    u256 rx, s;
    u256_from_be(rx, sig64, 32);
    u256_from_be(s, sig64 + 32, 32);

    /* Check s < n */
    if (u256_cmp(s, SECP256K1_N) >= 0) return 0;

    /* Lift public key x to point P (even y) */
    u256 px, py;
    if (!lift_x(px, py, pk32)) return 0;

    /* Compute challenge: e = tagged_hash("BIP0340/challenge", R.x || P.x || msg) mod n */
    /* Use dynamic allocation for variable-length message */
    uint8_t *challenge_buf = (uint8_t *)malloc(32 + 32 + msg_len);
    if (!challenge_buf) return 0;

    memcpy(challenge_buf, sig64, 32);       /* R.x */
    u256_to_be(challenge_buf + 32, px);      /* P.x */
    memcpy(challenge_buf + 64, msg, msg_len); /* msg */

    uint8_t e_hash[32];
    tagged_hash(e_hash, "BIP0340/challenge", challenge_buf, 64 + msg_len);
    free(challenge_buf);

    u256 e;
    u256_from_be(e, e_hash, 32);
    /* Reduce e mod n */
    while (u256_cmp(e, SECP256K1_N) >= 0) {
        u256_sub(e, e, SECP256K1_N);
    }

    /* Compute R' = s*G - e*P */
    /* BIP-340: s*G == R + e*P, so R = s*G - e*P */
    /* Negate e: n - e */
    u256 neg_e;
    mod_sub(neg_e, SECP256K1_N, e, SECP256K1_N);

    /* R' = s*G + neg_e*P using Shamir's trick */
    point_j R_check;
    scalar_mul_double(&R_check, s, neg_e, px, py);

    if (point_j_is_inf(&R_check)) return 0;

    /* Get affine coordinates */
    u256 Rx, Ry;
    if (!point_j_to_affine(Rx, Ry, &R_check)) return 0;

    /* BIP-340: R must have even y */
    if (Ry[0] & 1) return 0;

    /* Check R.x == rx from signature */
    if (u256_cmp(Rx, rx) != 0) return 0;

    /* Check R.x < p (should be guaranteed by affine conversion) */
    return 1;
}

/* ============================================================
 * Lean FFI Entry Points
 * ============================================================ */

lean_obj_res dion_secp256k1_ecdsa_verify(b_lean_obj_arg vk_obj, b_lean_obj_arg msg_obj,
                                              b_lean_obj_arg sig_obj) {
    uint8_t *vk = lean_sarray_cptr(vk_obj);
    size_t vk_len = lean_sarray_size(vk_obj);
    uint8_t *msg = lean_sarray_cptr(msg_obj);
    size_t msg_len = lean_sarray_size(msg_obj);
    uint8_t *sig = lean_sarray_cptr(sig_obj);
    size_t sig_len = lean_sarray_size(sig_obj);

    /* Validate input sizes: vk=33 (compressed), msg=32, sig=64 */
    if (vk_len != 33 || msg_len != 32 || sig_len != 64) {
        return lean_io_result_mk_ok(lean_box(0)); /* false */
    }

    int result = secp256k1_ecdsa_verify_impl(vk, msg, sig);
    return lean_io_result_mk_ok(lean_box(result ? 1 : 0));
}

lean_obj_res dion_secp256k1_schnorr_verify(b_lean_obj_arg vk_obj, b_lean_obj_arg msg_obj,
                                                b_lean_obj_arg sig_obj) {
    uint8_t *vk = lean_sarray_cptr(vk_obj);
    size_t vk_len = lean_sarray_size(vk_obj);
    uint8_t *msg = lean_sarray_cptr(msg_obj);
    size_t msg_len = lean_sarray_size(msg_obj);
    uint8_t *sig = lean_sarray_cptr(sig_obj);
    size_t sig_len = lean_sarray_size(sig_obj);

    /* Validate input sizes: vk=32 (x-only), msg=arbitrary, sig=64 */
    if (vk_len != 32 || sig_len != 64) {
        return lean_io_result_mk_ok(lean_box(0)); /* false */
    }

    int result = secp256k1_schnorr_verify_impl(vk, msg, msg_len, sig);
    return lean_io_result_mk_ok(lean_box(result ? 1 : 0));
}
