/*
 * Keccak-256 hashing for Plutus builtins (CIP-101)
 *
 * IMPORTANT: Keccak-256 is NOT SHA3-256.
 * Difference: Keccak-256 uses domain separator 0x01, SHA3-256 uses 0x06.
 * Ethereum uses Keccak-256 for its transaction hashes and Merkle Patricia Tries.
 *
 * This reuses the same Keccak-f[1600] permutation as sha3.c but with
 * the original Keccak padding (0x01) instead of SHA3 padding (0x06).
 */

#include <stdint.h>
#include <string.h>
#include <lean/lean.h>

/* Keccak-f[1600] round constants */
static const uint64_t keccak256_RC[24] = {
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

#define ROTL64(x, n) (((x) << (n)) | ((x) >> (64 - (n))))

static void keccak256_f1600(uint64_t state[25]) {
    for (int round = 0; round < 24; round++) {
        uint64_t C[5], D[5], B[25];
        for (int x = 0; x < 5; x++)
            C[x] = state[x] ^ state[x+5] ^ state[x+10] ^ state[x+15] ^ state[x+20];
        for (int x = 0; x < 5; x++) {
            D[x] = C[(x+4)%5] ^ ROTL64(C[(x+1)%5], 1);
            for (int y = 0; y < 25; y += 5)
                state[y+x] ^= D[x];
        }
        static const int keccak_piln[24] = {
            10,7,11,17,18,3,5,16,8,21,24,4,15,23,19,13,12,2,20,14,22,9,6,1
        };
        static const int keccak_rotc[24] = {
            1,3,6,10,15,21,28,36,45,55,2,14,27,41,56,8,25,43,62,18,39,61,20,44
        };
        uint64_t t = state[1];
        for (int i = 0; i < 24; i++) {
            int j = keccak_piln[i];
            B[0] = state[j];
            state[j] = ROTL64(t, keccak_rotc[i]);
            t = B[0];
        }
        for (int y = 0; y < 25; y += 5) {
            uint64_t t0=state[y+0], t1=state[y+1], t2=state[y+2], t3=state[y+3], t4=state[y+4];
            state[y+0] ^= (~t1) & t2;
            state[y+1] ^= (~t2) & t3;
            state[y+2] ^= (~t3) & t4;
            state[y+3] ^= (~t4) & t0;
            state[y+4] ^= (~t0) & t1;
        }
        state[0] ^= keccak256_RC[round];
    }
}

static void keccak_256_hash(const uint8_t *input, size_t inlen, uint8_t output[32]) {
    uint64_t state[25];
    memset(state, 0, sizeof(state));
    const size_t rate = 136; /* 1088 bits / 8 */
    size_t offset = 0;
    while (offset + rate <= inlen) {
        for (size_t i = 0; i < rate/8; i++)
            state[i] ^= ((const uint64_t*)(input + offset))[i];
        keccak256_f1600(state);
        offset += rate;
    }
    uint8_t temp[136];
    memset(temp, 0, rate);
    size_t remaining = inlen - offset;
    memcpy(temp, input + offset, remaining);
    temp[remaining] = 0x01;       /* Keccak domain separator (NOT SHA3's 0x06) */
    temp[rate - 1] |= 0x80;
    for (size_t i = 0; i < rate/8; i++)
        state[i] ^= ((uint64_t*)temp)[i];
    keccak256_f1600(state);
    memcpy(output, state, 32);
}

/*
 * dion_keccak_256 : ByteArray -> IO ByteArray
 */
lean_obj_res dion_keccak_256(b_lean_obj_arg data_obj, lean_obj_arg world) {
    (void)world;
    const uint8_t *data = lean_sarray_cptr(data_obj);
    size_t len = lean_sarray_size(data_obj);
    uint8_t hash[32];
    keccak_256_hash(data, len, hash);
    lean_object *result = lean_alloc_sarray(sizeof(uint8_t), 32, 32);
    memcpy(lean_sarray_cptr(result), hash, 32);
    return lean_io_result_mk_ok(result);
}
