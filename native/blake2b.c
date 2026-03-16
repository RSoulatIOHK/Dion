/*
 * Blake2b-256 hashing for Cardano
 *
 * Uses the reference Blake2b implementation for computing
 * transaction IDs and block hashes.
 */

#include <stdint.h>
#include <string.h>
#include <lean/lean.h>

// Blake2b state structure
typedef struct {
    uint64_t h[8];
    uint64_t t[2];
    uint64_t f[2];
    uint8_t buf[128];
    size_t buflen;
    size_t outlen;
} blake2b_state;

// Blake2b initialization vectors
static const uint64_t blake2b_IV[8] = {
    0x6a09e667f3bcc908ULL, 0xbb67ae8584caa73bULL,
    0x3c6ef372fe94f82bULL, 0xa54ff53a5f1d36f1ULL,
    0x510e527fade682d1ULL, 0x9b05688c2b3e6c1fULL,
    0x1f83d9abfb41bd6bULL, 0x5be0cd19137e2179ULL
};

// Rotation
static inline uint64_t rotr64(uint64_t x, int n) {
    return (x >> n) | (x << (64 - n));
}

// Blake2b mixing function
#define G(r, i, a, b, c, d)                         \
    do {                                            \
        a = a + b + m[blake2b_sigma[r][2*i+0]];    \
        d = rotr64(d ^ a, 32);                      \
        c = c + d;                                  \
        b = rotr64(b ^ c, 24);                      \
        a = a + b + m[blake2b_sigma[r][2*i+1]];    \
        d = rotr64(d ^ a, 16);                      \
        c = c + d;                                  \
        b = rotr64(b ^ c, 63);                      \
    } while(0)

static const uint8_t blake2b_sigma[12][16] = {
    { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
    { 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 },
    { 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 },
    { 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 },
    { 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 },
    { 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 },
    { 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 },
    { 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 },
    { 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 },
    { 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0 },
    { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
    { 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 }
};

static void blake2b_compress(blake2b_state *S, const uint8_t block[128]) {
    uint64_t m[16];
    uint64_t v[16];

    for (size_t i = 0; i < 16; ++i) {
        m[i] = ((uint64_t)block[i * 8 + 0]) |
               ((uint64_t)block[i * 8 + 1] << 8) |
               ((uint64_t)block[i * 8 + 2] << 16) |
               ((uint64_t)block[i * 8 + 3] << 24) |
               ((uint64_t)block[i * 8 + 4] << 32) |
               ((uint64_t)block[i * 8 + 5] << 40) |
               ((uint64_t)block[i * 8 + 6] << 48) |
               ((uint64_t)block[i * 8 + 7] << 56);
    }

    for (size_t i = 0; i < 8; ++i) {
        v[i] = S->h[i];
        v[i + 8] = blake2b_IV[i];
    }

    v[12] ^= S->t[0];
    v[13] ^= S->t[1];
    v[14] ^= S->f[0];
    v[15] ^= S->f[1];

    for (size_t r = 0; r < 12; ++r) {
        G(r, 0, v[0], v[4], v[8], v[12]);
        G(r, 1, v[1], v[5], v[9], v[13]);
        G(r, 2, v[2], v[6], v[10], v[14]);
        G(r, 3, v[3], v[7], v[11], v[15]);
        G(r, 4, v[0], v[5], v[10], v[15]);
        G(r, 5, v[1], v[6], v[11], v[12]);
        G(r, 6, v[2], v[7], v[8], v[13]);
        G(r, 7, v[3], v[4], v[9], v[14]);
    }

    for (size_t i = 0; i < 8; ++i) {
        S->h[i] ^= v[i] ^ v[i + 8];
    }
}

static void blake2b_init(blake2b_state *S, size_t outlen) {
    memset(S, 0, sizeof(blake2b_state));
    S->outlen = outlen;

    for (size_t i = 0; i < 8; ++i) {
        S->h[i] = blake2b_IV[i];
    }
    S->h[0] ^= 0x01010000 ^ outlen;
}

static void blake2b_update(blake2b_state *S, const uint8_t *in, size_t inlen) {
    while (inlen > 0) {
        size_t left = S->buflen;
        size_t fill = 128 - left;

        if (inlen > fill) {
            memcpy(S->buf + left, in, fill);
            S->buflen += fill;
            S->t[0] += 128;
            if (S->t[0] < 128) S->t[1]++;
            blake2b_compress(S, S->buf);
            S->buflen = 0;
            in += fill;
            inlen -= fill;
        } else {
            memcpy(S->buf + left, in, inlen);
            S->buflen += inlen;
            in += inlen;
            inlen = 0;
        }
    }
}

static void blake2b_final(blake2b_state *S, uint8_t *out) {
    S->t[0] += S->buflen;
    if (S->t[0] < S->buflen) S->t[1]++;
    S->f[0] = (uint64_t)-1;

    memset(S->buf + S->buflen, 0, 128 - S->buflen);
    blake2b_compress(S, S->buf);

    for (size_t i = 0; i < S->outlen; ++i) {
        out[i] = (S->h[i >> 3] >> (8 * (i & 7))) & 0xFF;
    }
}

/*
 * Compute Blake2b-256 hash
 * cleanode_blake2b_256 : ByteArray -> ByteArray
 */
lean_obj_res cleanode_blake2b_256(lean_obj_arg data_obj, lean_obj_arg world) {
    size_t len = lean_sarray_size(data_obj);
    const uint8_t* data = lean_sarray_cptr(data_obj);

    blake2b_state S;
    blake2b_init(&S, 32);  // 32 bytes = 256 bits
    blake2b_update(&S, data, len);

    uint8_t hash[32];
    blake2b_final(&S, hash);

    // Create ByteArray result
    lean_object* result = lean_alloc_sarray(sizeof(uint8_t), 32, 32);
    memcpy(lean_sarray_cptr(result), hash, 32);

    return lean_io_result_mk_ok(result);
}

/*
 * General-purpose Blake2b — called by kes.c and other native modules.
 * Matches the reference blake2b() signature.
 */
int blake2b(void *out, size_t outlen, const void *in, size_t inlen,
            const void *key, size_t keylen) {
    (void)key; (void)keylen;  /* Keyed hashing not used */
    blake2b_state S;
    blake2b_init(&S, outlen);
    blake2b_update(&S, (const uint8_t *)in, inlen);
    blake2b_final(&S, (uint8_t *)out);
    return 0;
}
