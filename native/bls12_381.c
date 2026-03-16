/*
 * BLS12-381 operations for Plutus builtins via blst library.
 *
 * Cardano Plutus BLS12-381 builtins operate on serialized points:
 *   G1: 48 bytes (compressed)
 *   G2: 96 bytes (compressed)
 *   MlResult (GT/Fp12): 576 bytes
 *
 * All functions return empty ByteArray on invalid input.
 *
 * References:
 *   - CIP-0381: Plutus support for BLS12-381 primitives
 *   - blst library: https://github.com/supranational/blst
 */

#include <stdint.h>
#include <string.h>
#include <lean/lean.h>
#include <blst.h>

static inline const uint8_t* ba_data(lean_obj_arg ba) {
    return lean_sarray_cptr(ba);
}

static inline size_t ba_size(lean_obj_arg ba) {
    return lean_sarray_size(ba);
}

static lean_obj_res mk_ba(const uint8_t *data, size_t len) {
    lean_object *r = lean_alloc_sarray(sizeof(uint8_t), len, len);
    memcpy(lean_sarray_cptr(r), data, len);
    return r;
}

static lean_obj_res mk_empty_ba(void) {
    return lean_alloc_sarray(sizeof(uint8_t), 0, 0);
}

#define FP12_BYTES (sizeof(blst_fp12))

/* ============================================================
 * G1 Operations
 * ============================================================ */

lean_obj_res cleanode_bls12_381_g1_add(lean_obj_arg a_obj, lean_obj_arg b_obj,
                                        lean_obj_arg world) {
    if (ba_size(a_obj) != 48 || ba_size(b_obj) != 48)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1_affine a_aff, b_aff;
    if (blst_p1_uncompress(&a_aff, ba_data(a_obj)) != BLST_SUCCESS ||
        blst_p1_uncompress(&b_aff, ba_data(b_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1 a_proj, b_proj, result;
    blst_p1_from_affine(&a_proj, &a_aff);
    blst_p1_from_affine(&b_proj, &b_aff);
    blst_p1_add_or_double(&result, &a_proj, &b_proj);

    uint8_t out[48];
    blst_p1_compress(out, &result);
    return lean_io_result_mk_ok(mk_ba(out, 48));
}

lean_obj_res cleanode_bls12_381_g1_neg(lean_obj_arg a_obj, lean_obj_arg world) {
    if (ba_size(a_obj) != 48)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1_affine a_aff;
    if (blst_p1_uncompress(&a_aff, ba_data(a_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1 a_proj;
    blst_p1_from_affine(&a_proj, &a_aff);
    blst_p1_cneg(&a_proj, 1);

    uint8_t out[48];
    blst_p1_compress(out, &a_proj);
    return lean_io_result_mk_ok(mk_ba(out, 48));
}

lean_obj_res cleanode_bls12_381_g1_scalar_mul(lean_obj_arg scalar_obj,
                                               lean_obj_arg point_obj,
                                               lean_obj_arg world) {
    if (ba_size(point_obj) != 48)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1_affine pt_aff;
    if (blst_p1_uncompress(&pt_aff, ba_data(point_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1 pt;
    blst_p1_from_affine(&pt, &pt_aff);

    blst_p1 result;
    blst_p1_mult(&result, &pt, ba_data(scalar_obj), ba_size(scalar_obj) * 8);

    uint8_t out[48];
    blst_p1_compress(out, &result);
    return lean_io_result_mk_ok(mk_ba(out, 48));
}

lean_obj_res cleanode_bls12_381_g1_equal(lean_obj_arg a_obj, lean_obj_arg b_obj,
                                          lean_obj_arg world) {
    if (ba_size(a_obj) != 48 || ba_size(b_obj) != 48)
        return lean_io_result_mk_ok(lean_box(0));

    blst_p1_affine a_aff, b_aff;
    if (blst_p1_uncompress(&a_aff, ba_data(a_obj)) != BLST_SUCCESS ||
        blst_p1_uncompress(&b_aff, ba_data(b_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(lean_box(0));

    blst_p1 a_proj, b_proj;
    blst_p1_from_affine(&a_proj, &a_aff);
    blst_p1_from_affine(&b_proj, &b_aff);

    return lean_io_result_mk_ok(lean_box(blst_p1_is_equal(&a_proj, &b_proj) ? 1 : 0));
}

lean_obj_res cleanode_bls12_381_g1_hash_to_group(lean_obj_arg msg_obj,
                                                   lean_obj_arg dst_obj,
                                                   lean_obj_arg world) {
    blst_p1 result;
    blst_hash_to_g1(&result, ba_data(msg_obj), ba_size(msg_obj),
                     ba_data(dst_obj), ba_size(dst_obj), NULL, 0);

    uint8_t out[48];
    blst_p1_compress(out, &result);
    return lean_io_result_mk_ok(mk_ba(out, 48));
}

lean_obj_res cleanode_bls12_381_g1_compress(lean_obj_arg a_obj, lean_obj_arg world) {
    if (ba_size(a_obj) == 48) {
        blst_p1_affine aff;
        if (blst_p1_uncompress(&aff, ba_data(a_obj)) != BLST_SUCCESS)
            return lean_io_result_mk_ok(mk_empty_ba());
        uint8_t out[48];
        blst_p1_affine_compress(out, &aff);
        return lean_io_result_mk_ok(mk_ba(out, 48));
    }
    if (ba_size(a_obj) != 96)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1_affine aff;
    if (blst_p1_deserialize(&aff, ba_data(a_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    uint8_t out[48];
    blst_p1_affine_compress(out, &aff);
    return lean_io_result_mk_ok(mk_ba(out, 48));
}

lean_obj_res cleanode_bls12_381_g1_uncompress(lean_obj_arg a_obj, lean_obj_arg world) {
    if (ba_size(a_obj) != 48)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1_affine aff;
    if (blst_p1_uncompress(&aff, ba_data(a_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    uint8_t out[96];
    blst_p1_affine_serialize(out, &aff);
    return lean_io_result_mk_ok(mk_ba(out, 96));
}

/* ============================================================
 * G2 Operations
 * ============================================================ */

lean_obj_res cleanode_bls12_381_g2_add(lean_obj_arg a_obj, lean_obj_arg b_obj,
                                        lean_obj_arg world) {
    if (ba_size(a_obj) != 96 || ba_size(b_obj) != 96)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2_affine a_aff, b_aff;
    if (blst_p2_uncompress(&a_aff, ba_data(a_obj)) != BLST_SUCCESS ||
        blst_p2_uncompress(&b_aff, ba_data(b_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2 a_proj, b_proj, result;
    blst_p2_from_affine(&a_proj, &a_aff);
    blst_p2_from_affine(&b_proj, &b_aff);
    blst_p2_add_or_double(&result, &a_proj, &b_proj);

    uint8_t out[96];
    blst_p2_compress(out, &result);
    return lean_io_result_mk_ok(mk_ba(out, 96));
}

lean_obj_res cleanode_bls12_381_g2_neg(lean_obj_arg a_obj, lean_obj_arg world) {
    if (ba_size(a_obj) != 96)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2_affine a_aff;
    if (blst_p2_uncompress(&a_aff, ba_data(a_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2 a_proj;
    blst_p2_from_affine(&a_proj, &a_aff);
    blst_p2_cneg(&a_proj, 1);

    uint8_t out[96];
    blst_p2_compress(out, &a_proj);
    return lean_io_result_mk_ok(mk_ba(out, 96));
}

lean_obj_res cleanode_bls12_381_g2_scalar_mul(lean_obj_arg scalar_obj,
                                               lean_obj_arg point_obj,
                                               lean_obj_arg world) {
    if (ba_size(point_obj) != 96)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2_affine pt_aff;
    if (blst_p2_uncompress(&pt_aff, ba_data(point_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2 pt;
    blst_p2_from_affine(&pt, &pt_aff);

    blst_p2 result;
    blst_p2_mult(&result, &pt, ba_data(scalar_obj), ba_size(scalar_obj) * 8);

    uint8_t out[96];
    blst_p2_compress(out, &result);
    return lean_io_result_mk_ok(mk_ba(out, 96));
}

lean_obj_res cleanode_bls12_381_g2_equal(lean_obj_arg a_obj, lean_obj_arg b_obj,
                                          lean_obj_arg world) {
    if (ba_size(a_obj) != 96 || ba_size(b_obj) != 96)
        return lean_io_result_mk_ok(lean_box(0));

    blst_p2_affine a_aff, b_aff;
    if (blst_p2_uncompress(&a_aff, ba_data(a_obj)) != BLST_SUCCESS ||
        blst_p2_uncompress(&b_aff, ba_data(b_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(lean_box(0));

    blst_p2 a_proj, b_proj;
    blst_p2_from_affine(&a_proj, &a_aff);
    blst_p2_from_affine(&b_proj, &b_aff);

    return lean_io_result_mk_ok(lean_box(blst_p2_is_equal(&a_proj, &b_proj) ? 1 : 0));
}

lean_obj_res cleanode_bls12_381_g2_hash_to_group(lean_obj_arg msg_obj,
                                                   lean_obj_arg dst_obj,
                                                   lean_obj_arg world) {
    blst_p2 result;
    blst_hash_to_g2(&result, ba_data(msg_obj), ba_size(msg_obj),
                     ba_data(dst_obj), ba_size(dst_obj), NULL, 0);

    uint8_t out[96];
    blst_p2_compress(out, &result);
    return lean_io_result_mk_ok(mk_ba(out, 96));
}

lean_obj_res cleanode_bls12_381_g2_compress(lean_obj_arg a_obj, lean_obj_arg world) {
    if (ba_size(a_obj) == 96) {
        blst_p2_affine aff;
        if (blst_p2_uncompress(&aff, ba_data(a_obj)) != BLST_SUCCESS)
            return lean_io_result_mk_ok(mk_empty_ba());
        uint8_t out[96];
        blst_p2_affine_compress(out, &aff);
        return lean_io_result_mk_ok(mk_ba(out, 96));
    }
    if (ba_size(a_obj) != 192)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2_affine aff;
    if (blst_p2_deserialize(&aff, ba_data(a_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    uint8_t out[96];
    blst_p2_affine_compress(out, &aff);
    return lean_io_result_mk_ok(mk_ba(out, 96));
}

lean_obj_res cleanode_bls12_381_g2_uncompress(lean_obj_arg a_obj, lean_obj_arg world) {
    if (ba_size(a_obj) != 96)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p2_affine aff;
    if (blst_p2_uncompress(&aff, ba_data(a_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    uint8_t out[192];
    blst_p2_affine_serialize(out, &aff);
    return lean_io_result_mk_ok(mk_ba(out, 192));
}

/* ============================================================
 * Pairing Operations
 * ============================================================ */

lean_obj_res cleanode_bls12_381_miller_loop(lean_obj_arg g1_obj, lean_obj_arg g2_obj,
                                             lean_obj_arg world) {
    if (ba_size(g1_obj) != 48 || ba_size(g2_obj) != 96)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_p1_affine g1_aff;
    blst_p2_affine g2_aff;
    if (blst_p1_uncompress(&g1_aff, ba_data(g1_obj)) != BLST_SUCCESS ||
        blst_p2_uncompress(&g2_aff, ba_data(g2_obj)) != BLST_SUCCESS)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_fp12 result;
    blst_miller_loop(&result, &g2_aff, &g1_aff);

    return lean_io_result_mk_ok(mk_ba((const uint8_t*)&result, FP12_BYTES));
}

lean_obj_res cleanode_bls12_381_mul_ml_result(lean_obj_arg a_obj, lean_obj_arg b_obj,
                                               lean_obj_arg world) {
    if (ba_size(a_obj) != FP12_BYTES || ba_size(b_obj) != FP12_BYTES)
        return lean_io_result_mk_ok(mk_empty_ba());

    blst_fp12 a, b, result;
    memcpy(&a, ba_data(a_obj), FP12_BYTES);
    memcpy(&b, ba_data(b_obj), FP12_BYTES);
    blst_fp12_mul(&result, &a, &b);

    return lean_io_result_mk_ok(mk_ba((const uint8_t*)&result, FP12_BYTES));
}

lean_obj_res cleanode_bls12_381_final_verify(lean_obj_arg a_obj, lean_obj_arg b_obj,
                                              lean_obj_arg world) {
    if (ba_size(a_obj) != FP12_BYTES || ba_size(b_obj) != FP12_BYTES)
        return lean_io_result_mk_ok(lean_box(0));

    blst_fp12 a, b;
    memcpy(&a, ba_data(a_obj), FP12_BYTES);
    memcpy(&b, ba_data(b_obj), FP12_BYTES);

    blst_fp12 fa, fb;
    blst_final_exp(&fa, &a);
    blst_final_exp(&fb, &b);

    int eq = (memcmp(&fa, &fb, FP12_BYTES) == 0);
    return lean_io_result_mk_ok(lean_box(eq ? 1 : 0));
}
