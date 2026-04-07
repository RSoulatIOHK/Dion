/*
 * RIPEMD-160 hashing for Plutus builtins (CIP-127)
 *
 * Used for Bitcoin address compatibility:
 *   Bitcoin P2PKH address = RIPEMD-160(SHA-256(pubkey))
 *
 * Reference: "RIPEMD-160: A Strengthened Version of RIPEMD"
 *            by Hans Dobbertin, Antoon Bosselaers, Bart Preneel
 */

#include <stdint.h>
#include <string.h>
#include <lean/lean.h>

/* Left rotation */
#define ROL32(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

/* RIPEMD-160 nonlinear functions */
#define F(x,y,z) ((x) ^ (y) ^ (z))
#define G(x,y,z) (((x) & (y)) | (~(x) & (z)))
#define H(x,y,z) (((x) | ~(y)) ^ (z))
#define I(x,y,z) (((x) & (z)) | ((y) & ~(z)))
#define J(x,y,z) ((x) ^ ((y) | ~(z)))

/* Round constants */
#define K0  0x00000000u
#define K1  0x5A827999u
#define K2  0x6ED9EBA1u
#define K3  0x8F1BBCDCu
#define K4  0xA953FD4Eu
#define KK0 0x50A28BE6u
#define KK1 0x5C4DD124u
#define KK2 0x6D703EF3u
#define KK3 0x7A6D76E9u
#define KK4 0x00000000u

/* Message word selection for left rounds */
static const int RL[80] = {
    0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
    7,4,13,1,10,6,15,3,12,0,9,5,2,14,11,8,
    3,10,14,4,9,15,8,1,2,7,0,6,13,11,5,12,
    1,9,11,10,0,8,12,4,13,3,7,15,14,5,6,2,
    4,0,5,9,7,12,2,10,14,1,3,8,11,6,15,13
};

/* Message word selection for right rounds */
static const int RR[80] = {
    5,14,7,0,9,2,11,4,13,6,15,8,1,10,3,12,
    6,11,3,7,0,13,5,10,14,15,8,12,4,9,1,2,
    15,5,1,3,7,14,6,9,11,8,12,2,10,0,4,13,
    8,6,4,1,3,11,15,0,5,12,2,13,9,7,10,14,
    12,15,10,4,1,5,8,7,6,2,13,14,0,3,9,11
};

/* Rotation amounts for left rounds */
static const int SL[80] = {
    11,14,15,12,5,8,7,9,11,13,14,15,6,7,9,8,
    7,6,8,13,11,9,7,15,7,12,15,9,11,7,13,12,
    11,13,6,7,14,9,13,15,14,8,13,6,5,12,7,5,
    11,12,14,15,14,15,9,8,9,14,5,6,8,6,5,12,
    9,15,5,11,6,8,13,12,5,12,13,14,11,8,5,6
};

/* Rotation amounts for right rounds */
static const int SR[80] = {
    8,9,9,11,13,15,15,5,7,7,8,11,14,14,12,6,
    9,13,15,7,12,8,9,11,7,7,12,7,6,15,13,11,
    9,7,15,11,8,6,6,14,12,13,5,14,13,13,7,5,
    15,5,8,11,14,14,6,14,6,9,12,9,12,5,15,8,
    8,5,12,9,12,5,14,6,8,13,6,5,15,13,11,11
};

static void ripemd160_compress(uint32_t state[5], const uint8_t block[64]) {
    uint32_t X[16];
    for (int i = 0; i < 16; i++)
        X[i] = (uint32_t)block[4*i] | ((uint32_t)block[4*i+1]<<8) |
               ((uint32_t)block[4*i+2]<<16) | ((uint32_t)block[4*i+3]<<24);

    uint32_t al=state[0], bl=state[1], cl=state[2], dl=state[3], el=state[4];
    uint32_t ar=state[0], br=state[1], cr=state[2], dr=state[3], er=state[4];

    for (int j = 0; j < 80; j++) {
        uint32_t fl, kl, fr, kr;
        if      (j < 16) { fl = F(bl,cl,dl); kl = K0; fr = J(br,cr,dr); kr = KK0; }
        else if (j < 32) { fl = G(bl,cl,dl); kl = K1; fr = I(br,cr,dr); kr = KK1; }
        else if (j < 48) { fl = H(bl,cl,dl); kl = K2; fr = H(br,cr,dr); kr = KK2; }
        else if (j < 64) { fl = I(bl,cl,dl); kl = K3; fr = G(br,cr,dr); kr = KK3; }
        else              { fl = J(bl,cl,dl); kl = K4; fr = F(br,cr,dr); kr = KK4; }

        uint32_t t;
        t = ROL32(al + fl + X[RL[j]] + kl, SL[j]) + el;
        al = el; el = dl; dl = ROL32(cl, 10); cl = bl; bl = t;

        t = ROL32(ar + fr + X[RR[j]] + kr, SR[j]) + er;
        ar = er; er = dr; dr = ROL32(cr, 10); cr = br; br = t;
    }

    uint32_t t = state[1] + cl + dr;
    state[1] = state[2] + dl + er;
    state[2] = state[3] + el + ar;
    state[3] = state[4] + al + br;
    state[4] = state[0] + bl + cr;
    state[0] = t;
}

static void ripemd160_hash(const uint8_t *msg, size_t len, uint8_t digest[20]) {
    uint32_t state[5] = { 0x67452301u, 0xEFCDAB89u, 0x98BADCFEu, 0x10325476u, 0xC3D2E1F0u };

    size_t offset = 0;
    while (offset + 64 <= len) {
        ripemd160_compress(state, msg + offset);
        offset += 64;
    }

    /* Padding */
    uint8_t block[64];
    memset(block, 0, 64);
    size_t remaining = len - offset;
    memcpy(block, msg + offset, remaining);
    block[remaining] = 0x80;

    if (remaining >= 56) {
        ripemd160_compress(state, block);
        memset(block, 0, 64);
    }
    uint64_t bits = (uint64_t)len * 8;
    memcpy(block + 56, &bits, 8);  /* Little-endian bit count */
    ripemd160_compress(state, block);

    for (int i = 0; i < 5; i++) {
        digest[4*i]   = (uint8_t)(state[i]);
        digest[4*i+1] = (uint8_t)(state[i] >> 8);
        digest[4*i+2] = (uint8_t)(state[i] >> 16);
        digest[4*i+3] = (uint8_t)(state[i] >> 24);
    }
}

/*
 * dion_ripemd_160 : ByteArray -> IO ByteArray
 */
lean_obj_res dion_ripemd_160(b_lean_obj_arg data_obj, lean_obj_arg world) {
    (void)world;
    const uint8_t *data = lean_sarray_cptr(data_obj);
    size_t len = lean_sarray_size(data_obj);
    uint8_t hash[20];
    ripemd160_hash(data, len, hash);
    lean_object *result = lean_alloc_sarray(sizeof(uint8_t), 20, 20);
    memcpy(lean_sarray_cptr(result), hash, 20);
    return lean_io_result_mk_ok(result);
}
