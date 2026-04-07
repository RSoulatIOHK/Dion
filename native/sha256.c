/*
 * SHA-256 hashing for Plutus script builtins
 *
 * Reference implementation of SHA-256 (FIPS 180-4).
 * No external dependencies.
 */

#include <stdint.h>
#include <string.h>
#include <lean/lean.h>

/* SHA-256 initial hash values (first 32 bits of fractional parts of
   square roots of the first 8 primes) */
static const uint32_t sha256_H0[8] = {
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
};

/* SHA-256 round constants (first 32 bits of fractional parts of
   cube roots of the first 64 primes) */
static const uint32_t sha256_K[64] = {
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

static inline uint32_t rotr32(uint32_t x, int n) {
    return (x >> n) | (x << (32 - n));
}

static inline uint32_t sha256_Ch(uint32_t x, uint32_t y, uint32_t z) {
    return (x & y) ^ (~x & z);
}

static inline uint32_t sha256_Maj(uint32_t x, uint32_t y, uint32_t z) {
    return (x & y) ^ (x & z) ^ (y & z);
}

static inline uint32_t sha256_Sigma0(uint32_t x) {
    return rotr32(x, 2) ^ rotr32(x, 13) ^ rotr32(x, 22);
}

static inline uint32_t sha256_Sigma1(uint32_t x) {
    return rotr32(x, 6) ^ rotr32(x, 11) ^ rotr32(x, 25);
}

static inline uint32_t sha256_sigma0(uint32_t x) {
    return rotr32(x, 7) ^ rotr32(x, 18) ^ (x >> 3);
}

static inline uint32_t sha256_sigma1(uint32_t x) {
    return rotr32(x, 17) ^ rotr32(x, 19) ^ (x >> 10);
}

typedef struct {
    uint32_t h[8];
    uint8_t buf[64];
    size_t buflen;
    uint64_t total;
} sha256_state;

static void sha256_init(sha256_state *S) {
    memcpy(S->h, sha256_H0, sizeof(sha256_H0));
    S->buflen = 0;
    S->total = 0;
}

static void sha256_compress(sha256_state *S, const uint8_t block[64]) {
    uint32_t W[64];
    uint32_t a, b, c, d, e, f, g, h;

    /* Prepare message schedule */
    for (int i = 0; i < 16; i++) {
        W[i] = ((uint32_t)block[i * 4 + 0] << 24) |
               ((uint32_t)block[i * 4 + 1] << 16) |
               ((uint32_t)block[i * 4 + 2] << 8)  |
               ((uint32_t)block[i * 4 + 3]);
    }
    for (int i = 16; i < 64; i++) {
        W[i] = sha256_sigma1(W[i-2]) + W[i-7] + sha256_sigma0(W[i-15]) + W[i-16];
    }

    /* Initialize working variables */
    a = S->h[0]; b = S->h[1]; c = S->h[2]; d = S->h[3];
    e = S->h[4]; f = S->h[5]; g = S->h[6]; h = S->h[7];

    /* 64 rounds */
    for (int i = 0; i < 64; i++) {
        uint32_t T1 = h + sha256_Sigma1(e) + sha256_Ch(e, f, g) + sha256_K[i] + W[i];
        uint32_t T2 = sha256_Sigma0(a) + sha256_Maj(a, b, c);
        h = g; g = f; f = e; e = d + T1;
        d = c; c = b; b = a; a = T1 + T2;
    }

    /* Update hash values */
    S->h[0] += a; S->h[1] += b; S->h[2] += c; S->h[3] += d;
    S->h[4] += e; S->h[5] += f; S->h[6] += g; S->h[7] += h;
}

static void sha256_update(sha256_state *S, const uint8_t *in, size_t inlen) {
    S->total += inlen;
    while (inlen > 0) {
        size_t left = S->buflen;
        size_t fill = 64 - left;
        if (inlen >= fill) {
            memcpy(S->buf + left, in, fill);
            sha256_compress(S, S->buf);
            S->buflen = 0;
            in += fill;
            inlen -= fill;
        } else {
            memcpy(S->buf + left, in, inlen);
            S->buflen += inlen;
            inlen = 0;
        }
    }
}

static void sha256_final(sha256_state *S, uint8_t out[32]) {
    uint64_t bits = S->total * 8;

    /* Padding: append 1 bit, then zeros, then 64-bit length (big-endian) */
    uint8_t pad = 0x80;
    sha256_update(S, &pad, 1);

    uint8_t zero = 0;
    while (S->buflen != 56) {
        sha256_update(S, &zero, 1);
    }

    /* Append length in bits as big-endian 64-bit */
    uint8_t len_bytes[8];
    for (int i = 7; i >= 0; i--) {
        len_bytes[i] = (uint8_t)(bits & 0xFF);
        bits >>= 8;
    }
    sha256_update(S, len_bytes, 8);

    /* Produce output (big-endian) */
    for (int i = 0; i < 8; i++) {
        out[i * 4 + 0] = (uint8_t)(S->h[i] >> 24);
        out[i * 4 + 1] = (uint8_t)(S->h[i] >> 16);
        out[i * 4 + 2] = (uint8_t)(S->h[i] >> 8);
        out[i * 4 + 3] = (uint8_t)(S->h[i]);
    }
}

/*
 * Compute SHA-256 hash
 * dion_sha256 : ByteArray -> IO ByteArray
 */
lean_obj_res dion_sha256(b_lean_obj_arg data_obj, lean_obj_arg world) {
    (void)world;
    uint8_t *data = lean_sarray_cptr(data_obj);
    size_t len = lean_sarray_size(data_obj);

    sha256_state S;
    sha256_init(&S);
    sha256_update(&S, data, len);

    uint8_t hash[32];
    sha256_final(&S, hash);

    lean_obj_res r = lean_alloc_sarray(1, 32, 32);
    memcpy(lean_sarray_cptr(r), hash, 32);
    return lean_io_result_mk_ok(r);
}
