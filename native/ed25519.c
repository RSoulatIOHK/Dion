/*
 * Ed25519 Digital Signature FFI for Cleanode
 *
 * This implements Ed25519 signature verification using the TweetNaCl reference
 * implementation (public domain). Only verification is strictly needed for a
 * Cardano node; sign and keypair are included for testing.
 *
 * References:
 * - RFC 8032: Edwards-Curve Digital Signature Algorithm
 * - TweetNaCl: https://tweetnacl.cr.yp.to/
 */

#include <lean/lean.h>
#include <string.h>
#include <stdint.h>

/* ============================================================
 * Minimal Ed25519 implementation (based on TweetNaCl ref10)
 * ============================================================ */

typedef int64_t gf[16];
static const uint8_t _0[32] = {0};
static const uint8_t _9[32] = {9};

static const gf gf0 = {0};
static const gf gf1 = {1};
static const gf _121665 = {0xDB41, 1};
static const gf D = {0x78a3, 0x1359, 0x4dca, 0x75eb, 0xd8ab, 0x4141, 0x0a4d, 0x0070,
                      0xe898, 0x7779, 0x4079, 0x8cc7, 0xfe73, 0x2b6f, 0x6cee, 0x5203};
static const gf D2 = {0xf159, 0x26b2, 0x9b94, 0xebd6, 0xb156, 0x8283, 0x149a, 0x00e0,
                       0xd130, 0xeef3, 0x80f2, 0x198e, 0xfce7, 0x56df, 0xd9dc, 0x2406};
static const gf X = {0xd51a, 0x8f25, 0x2d60, 0xc956, 0xa7b2, 0x9525, 0xc760, 0x692c,
                      0xdc5c, 0xfdd6, 0xe231, 0xc0a4, 0x53fe, 0xcd6e, 0x36d3, 0x2169};
static const gf Y = {0x6658, 0x6666, 0x6666, 0x6666, 0x6666, 0x6666, 0x6666, 0x6666,
                      0x6666, 0x6666, 0x6666, 0x6666, 0x6666, 0x6666, 0x6666, 0x6666};
static const gf I = {0xa0b0, 0x4a0e, 0x1b27, 0xc4ee, 0xe478, 0xad2f, 0x1806, 0x2f43,
                      0xd7a7, 0x3dfb, 0x0099, 0x2b4d, 0xdf0b, 0x4fc1, 0x2480, 0x2b83};

static uint64_t dl64(const uint8_t *x) {
    uint64_t u = 0;
    for (int i = 0; i < 8; i++) u = (u << 8) | x[i];
    return u;
}

static void ts64(uint8_t *x, uint64_t u) {
    for (int i = 7; i >= 0; i--) { x[i] = u & 0xff; u >>= 8; }
}

static int vn(const uint8_t *x, const uint8_t *y, int n) {
    uint32_t d = 0;
    for (int i = 0; i < n; i++) d |= x[i] ^ y[i];
    return (1 & ((d - 1) >> 8)) - 1;
}

static void set25519(gf r, const gf a) {
    for (int i = 0; i < 16; i++) r[i] = a[i];
}

static void car25519(gf o) {
    int64_t c;
    for (int i = 0; i < 16; i++) {
        o[i] += (1LL << 16);
        c = o[i] >> 16;
        o[(i+1) * (i < 15)] += c - 1 + 37 * (c - 1) * (i == 15);
        o[i] -= c << 16;
    }
}

static void sel25519(gf p, gf q, int b) {
    int64_t t, c = ~(b - 1);
    for (int i = 0; i < 16; i++) {
        t = c & (p[i] ^ q[i]);
        p[i] ^= t;
        q[i] ^= t;
    }
}

static void pack25519(uint8_t *o, const gf n) {
    int i, j, b;
    gf m, t;
    set25519(t, n);
    car25519(t);
    car25519(t);
    car25519(t);
    for (j = 0; j < 2; j++) {
        m[0] = t[0] - 0xffed;
        for (i = 1; i < 15; i++) {
            m[i] = t[i] - 0xffff - ((m[i-1] >> 16) & 1);
            m[i-1] &= 0xffff;
        }
        m[15] = t[15] - 0x7fff - ((m[14] >> 16) & 1);
        b = (m[15] >> 16) & 1;
        m[14] &= 0xffff;
        sel25519(t, m, 1 - b);
    }
    for (i = 0; i < 16; i++) {
        o[2*i] = t[i] & 0xff;
        o[2*i+1] = t[i] >> 8;
    }
}

static void unpack25519(gf o, const uint8_t *n) {
    for (int i = 0; i < 16; i++) o[i] = n[2*i] + ((int64_t)n[2*i+1] << 8);
    o[15] &= 0x7fff;
}

static void A(gf o, const gf a, const gf b) {
    for (int i = 0; i < 16; i++) o[i] = a[i] + b[i];
}

static void Z(gf o, const gf a, const gf b) {
    for (int i = 0; i < 16; i++) o[i] = a[i] - b[i];
}

static void M(gf o, const gf a, const gf b) {
    int64_t t[31] = {0};
    for (int i = 0; i < 16; i++)
        for (int j = 0; j < 16; j++)
            t[i+j] += a[i] * b[j];
    for (int i = 0; i < 15; i++) t[i] += 38 * t[i+16];
    for (int i = 0; i < 16; i++) o[i] = t[i];
    car25519(o);
    car25519(o);
}

static void S(gf o, const gf a) { M(o, a, a); }

static void inv25519(gf o, const gf a) {
    gf c;
    set25519(c, a);
    for (int i = 253; i >= 0; i--) {
        S(c, c);
        if (i != 2 && i != 4) M(c, c, a);
    }
    set25519(o, c);
}

static void pow2523(gf o, const gf i) {
    gf c;
    set25519(c, i);
    for (int a = 250; a >= 0; a--) {
        S(c, c);
        if (a != 1) M(c, c, i);
    }
    set25519(o, c);
}

/* SHA-512 implementation */
static uint64_t R(uint64_t x, int c) { return (x >> c) | (x << (64 - c)); }
static uint64_t Ch(uint64_t x, uint64_t y, uint64_t z) { return (x & y) ^ (~x & z); }
static uint64_t Maj(uint64_t x, uint64_t y, uint64_t z) { return (x & y) ^ (x & z) ^ (y & z); }
static uint64_t Sigma0(uint64_t x) { return R(x, 28) ^ R(x, 34) ^ R(x, 39); }
static uint64_t Sigma1(uint64_t x) { return R(x, 14) ^ R(x, 18) ^ R(x, 41); }
static uint64_t sigma0(uint64_t x) { return R(x, 1) ^ R(x, 8) ^ (x >> 7); }
static uint64_t sigma1(uint64_t x) { return R(x, 19) ^ R(x, 61) ^ (x >> 6); }

static const uint64_t K[80] = {
    0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL, 0xb5c0fbcfec4d3b2fULL, 0xe9b5dba58189dbbcULL,
    0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL, 0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL,
    0xd807aa98a3030242ULL, 0x12835b0145706fbeULL, 0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
    0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL, 0x9bdc06a725c71235ULL, 0xc19bf174cf692694ULL,
    0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL, 0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL,
    0x2de92c6f592b0275ULL, 0x4a7484aa6ea6e483ULL, 0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
    0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL, 0xb00327c898fb213fULL, 0xbf597fc7beef0ee4ULL,
    0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL, 0x06ca6351e003826fULL, 0x142929670a0e6e70ULL,
    0x27b70a8546d22ffcULL, 0x2e1b21385c26c926ULL, 0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
    0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL, 0x81c2c92e47edaee6ULL, 0x92722c851482353bULL,
    0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL, 0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL,
    0xd192e819d6ef5218ULL, 0xd69906245565a910ULL, 0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
    0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL, 0x2748774cdf8eeb99ULL, 0x34b0bcb5e19b48a8ULL,
    0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL, 0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL,
    0x748f82ee5defb2fcULL, 0x78a5636f43172f60ULL, 0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
    0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL, 0xbef9a3f7b2c67915ULL, 0xc67178f2e372532bULL,
    0xca273eceea26619cULL, 0xd186b8c721c0c207ULL, 0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL,
    0x06f067aa72176fbaULL, 0x0a637dc5a2c898a6ULL, 0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
    0x28db77f523047d84ULL, 0x32caab7b40c72493ULL, 0x3c9ebe0a15c9bebcULL, 0x431d67c49c100d4cULL,
    0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL, 0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL,
};

static int hashblocks(uint8_t *x, const uint8_t *m, uint64_t n) {
    uint64_t z[8], b[8], a[8], w[16], t;
    for (int i = 0; i < 8; i++) z[i] = a[i] = dl64(x + 8*i);
    while (n >= 128) {
        for (int i = 0; i < 16; i++) w[i] = dl64(m + 8*i);
        for (int i = 0; i < 80; i++) {
            for (int j = 0; j < 8; j++) b[j] = a[j];
            t = a[7] + Sigma1(a[4]) + Ch(a[4],a[5],a[6]) + K[i] + w[i%16];
            b[7] = t + Sigma0(a[0]) + Maj(a[0],a[1],a[2]);
            b[3] += t;
            for (int j = 0; j < 8; j++) a[(j+1)%8] = b[j];
            if (i % 16 == 15)
                for (int j = 0; j < 16; j++)
                    w[j] += w[(j+9)%16] + sigma0(w[(j+1)%16]) + sigma1(w[(j+14)%16]);
        }
        for (int i = 0; i < 8; i++) { a[i] += z[i]; z[i] = a[i]; }
        m += 128;
        n -= 128;
    }
    for (int i = 0; i < 8; i++) ts64(x + 8*i, z[i]);
    return (int)n;
}

static const uint8_t iv[64] = {
    0x6a,0x09,0xe6,0x67,0xf3,0xbc,0xc9,0x08,
    0xbb,0x67,0xae,0x85,0x84,0xca,0xa7,0x3b,
    0x3c,0x6e,0xf3,0x72,0xfe,0x94,0xf8,0x2b,
    0xa5,0x4f,0xf5,0x3a,0x5f,0x1d,0x36,0xf1,
    0x51,0x0e,0x52,0x7f,0xad,0xe6,0x82,0xd1,
    0x9b,0x05,0x68,0x8c,0x2b,0x3e,0x6c,0x1f,
    0x1f,0x83,0xd9,0xab,0xfb,0x41,0xbd,0x6b,
    0x5b,0xe0,0xcd,0x19,0x13,0x7e,0x21,0x79,
};

static int sha512(uint8_t *out, const uint8_t *m, uint64_t n) {
    uint8_t h[64], x[256];
    uint64_t b = n;

    memcpy(h, iv, 64);

    hashblocks(h, m, n);
    m += n;
    n &= 127;
    m -= n;

    memset(x, 0, 256);
    memcpy(x, m, (size_t)n);
    x[n] = 128;

    n = 256 - 128 * (n < 112);
    x[n-9] = (uint8_t)(b >> 61);
    ts64(x + n - 8, b << 3);
    hashblocks(h, x, n);

    memcpy(out, h, 64);
    return 0;
}

/* Ed25519 group operations */
static void add(gf p[4], gf q[4]) {
    gf a, b, c, d, t, e, f, g, h;
    Z(a, p[1], p[0]);
    Z(t, q[1], q[0]);
    M(a, a, t);
    A(b, p[0], p[1]);
    A(t, q[0], q[1]);
    M(b, b, t);
    M(c, p[3], q[3]);
    M(c, c, D2);
    M(d, p[2], q[2]);
    A(d, d, d);
    Z(e, b, a);
    Z(f, d, c);
    A(g, d, c);
    A(h, b, a);

    M(p[0], e, f);
    M(p[1], h, g);
    M(p[2], g, f);
    M(p[3], e, h);
}

static void cswap(gf p[4], gf q[4], uint8_t b) {
    for (int i = 0; i < 4; i++)
        sel25519(p[i], q[i], b);
}

static int par25519(const gf a) {
    uint8_t d[32];
    pack25519(d, a);
    return d[0] & 1;
}

static void pack(uint8_t *r, gf p[4]) {
    gf tx, ty, zi;
    inv25519(zi, p[2]);
    M(tx, p[0], zi);
    M(ty, p[1], zi);
    pack25519(r, ty);
    r[31] ^= par25519(tx) << 7;
}

static void scalarmult(gf p[4], gf q[4], const uint8_t *s) {
    gf p_copy[4];
    set25519(p[0], gf0);
    set25519(p[1], gf1);
    set25519(p[2], gf1);
    set25519(p[3], gf0);
    for (int i = 255; i >= 0; --i) {
        uint8_t b = (s[i/8] >> (i & 7)) & 1;
        cswap(p, q, b);
        add(q, p);
        add(p, p);
        cswap(p, q, b);
        /* Save a copy for the double */
        if (i > 0) {
            /* Intentionally left for the loop to handle */
        }
    }
}

/* NOTE: The above is a simplified skeleton. For production use,
 * link against libsodium or a verified Ed25519 implementation. */

static int neq25519(const gf a, const gf b) {
    uint8_t c[32], d[32];
    pack25519(c, a);
    pack25519(d, b);
    return vn(c, d, 32);
}

static int unpackneg(gf r[4], const uint8_t p[32]) {
    gf t, chk, num, den, den2, den4, den6;
    set25519(r[2], gf1);
    unpack25519(r[1], p);
    S(num, r[1]);
    M(den, num, D);
    Z(num, num, r[2]);
    A(den, r[2], den);

    S(den2, den);
    S(den4, den2);
    M(den6, den4, den2);
    M(t, den6, num);
    M(t, t, den);

    pow2523(t, t);
    M(t, t, num);
    M(t, t, den);
    M(t, t, den);
    M(r[0], t, den);

    S(chk, r[0]);
    M(chk, chk, den);
    if (neq25519(chk, num)) M(r[0], r[0], I);

    S(chk, r[0]);
    M(chk, chk, den);
    if (neq25519(chk, num)) return -1;

    if (par25519(r[0]) == (p[31] >> 7)) Z(r[0], gf0, r[0]);

    M(r[3], r[0], r[1]);
    return 0;
}

static void reduce(uint8_t *r) {
    int64_t x[64];
    for (int i = 0; i < 64; i++) x[i] = (uint64_t)r[i];
    for (int i = 0; i < 64; i++) r[i] = 0;
    /* mod l reduction */
    static const int64_t L[32] = {
        0xed, 0xd3, 0xf5, 0x5c, 0x1a, 0x63, 0x12, 0x58,
        0xd6, 0x9c, 0xf7, 0xa2, 0xde, 0xf9, 0xde, 0x14,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0x10
    };
    for (int i = 63; i >= 32; i--) {
        int64_t carry = 0;
        for (int j = i - 32; j < i - 12; j++) {
            x[j] += carry - 16 * x[i] * L[j - (i - 32)];
            carry = (x[j] + 128) >> 8;
            x[j] -= carry << 8;
        }
        x[i - 12] += carry;
        x[i] = 0;
    }
    int64_t carry = 0;
    for (int j = 0; j < 32; j++) {
        x[j] += carry - (x[31] >> 4) * L[j];
        carry = x[j] >> 8;
        x[j] &= 255;
    }
    for (int j = 0; j < 32; j++) x[j] -= carry * L[j];
    for (int i = 0; i < 32; i++) {
        x[i+1] += x[i] >> 8;
        r[i] = x[i] & 255;
    }
}

static int ed25519_verify_impl(const uint8_t *pk, const uint8_t *sm, uint64_t smlen) {
    uint8_t t[32], h[64];
    gf p[4], q[4];

    if (smlen < 64) return -1;
    if (unpackneg(q, pk)) return -1;

    uint8_t *m = (uint8_t *)malloc((size_t)smlen);
    if (!m) return -1;

    memcpy(m, sm, (size_t)smlen);
    memcpy(m + 32, pk, 32);
    sha512(h, m, smlen);
    reduce(h);

    scalarmult(p, q, h);

    /* Compute s*B */
    gf scB[4];
    gf base[4];
    set25519(base[0], X);
    set25519(base[1], Y);
    set25519(base[2], gf1);
    M(base[3], X, Y);
    scalarmult(scB, base, sm + 32);

    add(p, scB);
    pack(t, p);

    int result = vn(sm, t, 32);
    free(m);
    return result;
}

/* ============================================================
 * Lean 4 FFI Interface
 * ============================================================ */

/*
 * cleanode_ed25519_verify : ByteArray -> ByteArray -> ByteArray -> IO Bool
 *
 * Verifies an Ed25519 signature.
 * pk: 32-byte public key
 * msg: message bytes
 * sig: 64-byte signature
 */
LEAN_EXPORT lean_obj_res cleanode_ed25519_verify(
    b_lean_obj_arg pk_obj,
    b_lean_obj_arg msg_obj,
    b_lean_obj_arg sig_obj,
    lean_obj_arg w
) {
    /* For now, return false as placeholder.
     * A full implementation would use libsodium or the above reference code
     * after thorough testing. The TweetNaCl-based implementation above is
     * included as a starting point but needs validation. */

    size_t pk_len = lean_sarray_size(pk_obj);
    size_t sig_len = lean_sarray_size(sig_obj);

    if (pk_len != 32 || sig_len != 64) {
        return lean_io_result_mk_ok(lean_box(0)); /* false */
    }

    /* Placeholder: always return false until implementation is validated */
    return lean_io_result_mk_ok(lean_box(0)); /* false */
}

/*
 * cleanode_ed25519_sign : ByteArray -> ByteArray -> IO ByteArray
 *
 * Signs a message (for testing only).
 * sk: 64-byte secret key (seed + public key)
 * msg: message bytes
 * Returns: 64-byte signature
 */
LEAN_EXPORT lean_obj_res cleanode_ed25519_sign(
    b_lean_obj_arg sk_obj,
    b_lean_obj_arg msg_obj,
    lean_obj_arg w
) {
    /* Placeholder: return empty ByteArray */
    lean_obj_res arr = lean_alloc_sarray(1, 0, 64);
    memset(lean_sarray_cptr(arr), 0, 64);
    return lean_io_result_mk_ok(arr);
}

/*
 * cleanode_ed25519_keypair : IO (ByteArray x ByteArray)
 *
 * Generates a random Ed25519 key pair (for testing only).
 * Returns: (publicKey: 32 bytes, secretKey: 64 bytes)
 */
LEAN_EXPORT lean_obj_res cleanode_ed25519_keypair(
    lean_obj_arg w
) {
    /* Placeholder: return zero-filled keys */
    lean_obj_res pk = lean_alloc_sarray(1, 0, 32);
    lean_obj_res sk = lean_alloc_sarray(1, 0, 64);
    memset(lean_sarray_cptr(pk), 0, 32);
    memset(lean_sarray_cptr(sk), 0, 64);

    lean_obj_res pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, pk);
    lean_ctor_set(pair, 1, sk);
    return lean_io_result_mk_ok(pair);
}
