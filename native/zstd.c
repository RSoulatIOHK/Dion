/*
 * Zstandard Decompression FFI for Cleanode
 *
 * Provides streaming decompression of zstd-compressed files.
 * Used by Mithril integration for snapshot decompression.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zstd.h>
#include <lean/lean.h>

/* ========================
 * Lean FFI helpers
 * ======================== */

static lean_obj_res mk_except_ok_zstd(lean_obj_arg val) {
    lean_object *r = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(r, 0, val);
    return r;
}

static lean_obj_res mk_except_error_zstd(lean_obj_arg val) {
    lean_object *r = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(r, 0, val);
    return r;
}

/* ========================
 * Streaming decompression
 * ======================== */

/*
 * Decompress a zstd-compressed file to an output file.
 * Uses streaming decompression to handle files larger than memory.
 *
 * @param src_path  Path to the compressed input file
 * @param dst_path  Path to write the decompressed output
 * @return IO (Except String Unit)
 */
LEAN_EXPORT lean_obj_res dion_zstd_decompress_file(
    b_lean_obj_arg src_obj, b_lean_obj_arg dst_obj, lean_obj_arg world)
{
    const char *src_path = lean_string_cstr(src_obj);
    const char *dst_path = lean_string_cstr(dst_obj);

    FILE *fin = fopen(src_path, "rb");
    if (!fin) {
        char errbuf[512];
        snprintf(errbuf, sizeof(errbuf), "Cannot open input file: %s", src_path);
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string(errbuf)));
    }

    FILE *fout = fopen(dst_path, "wb");
    if (!fout) {
        fclose(fin);
        char errbuf[512];
        snprintf(errbuf, sizeof(errbuf), "Cannot open output file: %s", dst_path);
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string(errbuf)));
    }

    ZSTD_DCtx *dctx = ZSTD_createDCtx();
    if (!dctx) {
        fclose(fin);
        fclose(fout);
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Failed to create ZSTD decompression context")));
    }

    size_t const buffInSize = ZSTD_DStreamInSize();
    size_t const buffOutSize = ZSTD_DStreamOutSize();
    void *buffIn = malloc(buffInSize);
    void *buffOut = malloc(buffOutSize);

    if (!buffIn || !buffOut) {
        free(buffIn);
        free(buffOut);
        ZSTD_freeDCtx(dctx);
        fclose(fin);
        fclose(fout);
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Failed to allocate decompression buffers")));
    }

    size_t lastRet = 0;
    int isEmpty = 1;

    while (1) {
        size_t read = fread(buffIn, 1, buffInSize, fin);
        if (read == 0) break;
        isEmpty = 0;

        ZSTD_inBuffer input = { buffIn, read, 0 };
        while (input.pos < input.size) {
            ZSTD_outBuffer output = { buffOut, buffOutSize, 0 };
            size_t const ret = ZSTD_decompressStream(dctx, &output, &input);
            if (ZSTD_isError(ret)) {
                char errbuf[512];
                snprintf(errbuf, sizeof(errbuf), "Decompression error: %s", ZSTD_getErrorName(ret));
                free(buffIn);
                free(buffOut);
                ZSTD_freeDCtx(dctx);
                fclose(fin);
                fclose(fout);
                return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string(errbuf)));
            }
            fwrite(buffOut, 1, output.pos, fout);
            lastRet = ret;
        }
    }

    free(buffIn);
    free(buffOut);
    ZSTD_freeDCtx(dctx);
    fclose(fin);
    fclose(fout);

    if (isEmpty) {
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Input file is empty")));
    }

    /* lastRet != 0 means the frame is incomplete */
    if (lastRet != 0) {
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Decompression incomplete: truncated input")));
    }

    return lean_io_result_mk_ok(mk_except_ok_zstd(lean_box(0)));
}

/*
 * Decompress a zstd-compressed ByteArray in memory.
 *
 * @param data  The compressed ByteArray
 * @return IO (Except String ByteArray)
 */
LEAN_EXPORT lean_obj_res dion_zstd_decompress(b_lean_obj_arg data_obj, lean_obj_arg world) {
    size_t srcSize = lean_sarray_size(data_obj);
    const void *src = lean_sarray_cptr(data_obj);

    /* Get decompressed size if available in frame header */
    unsigned long long const rSize = ZSTD_getFrameContentSize(src, srcSize);
    if (rSize == ZSTD_CONTENTSIZE_ERROR) {
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Not a valid zstd frame")));
    }

    size_t dstCapacity;
    if (rSize == ZSTD_CONTENTSIZE_UNKNOWN) {
        /* Unknown size — use 4x source as initial estimate */
        dstCapacity = srcSize * 4;
        if (dstCapacity < 65536) dstCapacity = 65536;
    } else {
        dstCapacity = (size_t)rSize;
    }

    /* For known-size frames, single-shot decompress */
    if (rSize != ZSTD_CONTENTSIZE_UNKNOWN) {
        lean_object *result = lean_alloc_sarray(1, dstCapacity, dstCapacity);
        size_t const dSize = ZSTD_decompress(lean_sarray_cptr(result), dstCapacity, src, srcSize);
        if (ZSTD_isError(dSize)) {
            char errbuf[256];
            snprintf(errbuf, sizeof(errbuf), "Decompression error: %s", ZSTD_getErrorName(dSize));
            return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string(errbuf)));
        }
        return lean_io_result_mk_ok(mk_except_ok_zstd(result));
    }

    /* Streaming decompression for unknown-size frames */
    ZSTD_DCtx *dctx = ZSTD_createDCtx();
    if (!dctx) {
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Failed to create ZSTD context")));
    }

    void *dst = malloc(dstCapacity);
    if (!dst) {
        ZSTD_freeDCtx(dctx);
        return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Failed to allocate output buffer")));
    }

    ZSTD_inBuffer input = { src, srcSize, 0 };
    size_t totalOut = 0;

    while (input.pos < input.size) {
        if (totalOut >= dstCapacity) {
            dstCapacity *= 2;
            void *newDst = realloc(dst, dstCapacity);
            if (!newDst) {
                free(dst);
                ZSTD_freeDCtx(dctx);
                return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string("Failed to grow output buffer")));
            }
            dst = newDst;
        }
        ZSTD_outBuffer output = { (char *)dst + totalOut, dstCapacity - totalOut, 0 };
        size_t const ret = ZSTD_decompressStream(dctx, &output, &input);
        if (ZSTD_isError(ret)) {
            char errbuf[256];
            snprintf(errbuf, sizeof(errbuf), "Decompression error: %s", ZSTD_getErrorName(ret));
            free(dst);
            ZSTD_freeDCtx(dctx);
            return lean_io_result_mk_ok(mk_except_error_zstd(lean_mk_string(errbuf)));
        }
        totalOut += output.pos;
    }

    ZSTD_freeDCtx(dctx);

    lean_object *result = lean_alloc_sarray(1, totalOut, totalOut);
    memcpy(lean_sarray_cptr(result), dst, totalOut);
    free(dst);

    return lean_io_result_mk_ok(mk_except_ok_zstd(result));
}
