/*
 * SHA3-256 (Keccak) hashing for Plutus script builtins
 *
 * Reference implementation of SHA3-256 (FIPS 202).
 * Implements Keccak-f[1600] permutation with SHA3-256 parameters:
 *   rate = 1088 bits (136 bytes), capacity = 512 bits, output = 256 bits.
 * Domain separator: 0x06 (SHA3 standard, NOT raw Keccak 0x01).
 * No external dependencies.
 */

#include <stdint.h>
#include <string.h>
#include <lean/lean.h>

/* Keccak-f[1600] round constants */
static const uint64_t keccak_RC[24] = {
    0x0000000000000001ULL, 0x0000000000008082ULL,
    0x800000000000808aULL, 0x8000000080008000ULL,
    0x000000000000808bULL, 0x0000000080000001ULL,
    0x8000000080008081ULL, 0x8000000000008009ULL,
    0x000000000000008aULL, 0x0000000000000088ULL,
    0x0000000080008009ULL, 0x000000008000000aULL,
    0x000000008000808bULL, 0x800000000000008bULL,
    0x8000000000008089ULL, 0x8000000000008003ULL,
    0x8000000000008002ULL, 0x8000000000000080ULL,
    0x000000000000800aULL, 0x800000008000000aULL,
    0x8000000080008081ULL, 0x8000000000008080ULL,
    0x0000000080000001ULL, 0x8000000080008008ULL
};

/* Rotation offsets for rho step */
static const int keccak_rotc[24] = {
     1,  3,  6, 10, 15, 21, 28, 36,
    45, 55,  2, 14, 27, 41, 56,  8,
    25, 43, 62, 18, 39, 21, 56, 14
};

/* Lane indices for pi step */
static const int keccak_piln[24] = {
    10,  7, 11, 17, 18,  3,  5, 16,
     8, 21, 24,  4, 15, 23, 19, 13,
    12,  2, 20, 14, 22,  9,  6,  1
};

static inline uint64_t rotl64(uint64_t x, int n) {
    return (x << n) | (x >> (64 - n));
}

/* Keccak-f[1600] permutation */
static void keccakf(uint64_t st[25]) {
    uint64_t t, bc[5];

    for (int round = 0; round < 24; round++) {
        /* Theta */
        for (int i = 0; i < 5; i++)
            bc[i] = st[i] ^ st[i + 5] ^ st[i + 10] ^ st[i + 15] ^ st[i + 20];
        for (int i = 0; i < 5; i++) {
            t = bc[(i + 4) % 5] ^ rotl64(bc[(i + 1) % 5], 1);
            for (int j = 0; j < 25; j += 5)
                st[j + i] ^= t;
        }

        /* Rho and Pi */
        t = st[1];
        for (int i = 0; i < 24; i++) {
            int j = keccak_piln[i];
            bc[0] = st[j];
            st[j] = rotl64(t, keccak_rotc[i]);
            t = bc[0];
        }

        /* Chi */
        for (int j = 0; j < 25; j += 5) {
            for (int i = 0; i < 5; i++)
                bc[i] = st[j + i];
            for (int i = 0; i < 5; i++)
                st[j + i] ^= (~bc[(i + 1) % 5]) & bc[(i + 2) % 5];
        }

        /* Iota */
        st[0] ^= keccak_RC[round];
    }
}

/*
 * SHA3-256 hash computation
 *   rate = 136 bytes, output = 32 bytes, domain separator = 0x06
 */
static void sha3_256(const uint8_t *in, size_t inlen, uint8_t out[32]) {
    uint64_t st[25];
    uint8_t temp[136]; /* rate in bytes */
    size_t rate = 136;

    memset(st, 0, sizeof(st));

    /* Absorb full blocks */
    while (inlen >= rate) {
        for (size_t i = 0; i < rate / 8; i++) {
            uint64_t lane = 0;
            for (int b = 0; b < 8; b++)
                lane |= (uint64_t)in[i * 8 + b] << (8 * b);
            st[i] ^= lane;
        }
        keccakf(st);
        in += rate;
        inlen -= rate;
    }

    /* Absorb final block with padding */
    memset(temp, 0, rate);
    memcpy(temp, in, inlen);
    temp[inlen] = 0x06;       /* SHA3 domain separator */
    temp[rate - 1] |= 0x80;  /* Final padding bit */

    for (size_t i = 0; i < rate / 8; i++) {
        uint64_t lane = 0;
        for (int b = 0; b < 8; b++)
            lane |= (uint64_t)temp[i * 8 + b] << (8 * b);
        st[i] ^= lane;
    }
    keccakf(st);

    /* Squeeze: extract 32 bytes of output (fits in one block since 32 < 136) */
    for (size_t i = 0; i < 32; i++) {
        out[i] = (uint8_t)(st[i / 8] >> (8 * (i % 8)));
    }
}

/*
 * Compute SHA3-256 hash
 * cleanode_sha3_256 : ByteArray -> IO ByteArray
 */
lean_obj_res cleanode_sha3_256(b_lean_obj_arg data_obj, lean_obj_arg world) {
    (void)world;
    uint8_t *data = lean_sarray_cptr(data_obj);
    size_t len = lean_sarray_size(data_obj);

    uint8_t hash[32];
    sha3_256(data, len, hash);

    lean_obj_res r = lean_alloc_sarray(1, 32, 32);
    memcpy(lean_sarray_cptr(r), hash, 32);
    return lean_io_result_mk_ok(r);
}
