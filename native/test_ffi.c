#include <lean/lean.h>
#include <sys/time.h>

// Simple test function that returns an integer
lean_obj_res cleanode_test_simple(uint32_t x, lean_obj_arg world) {
    uint32_t result = x + 42;
    lean_object* res = lean_box_uint32(result);
    return lean_io_result_mk_ok(res);
}

// Get current Unix time in seconds (for slot clock)
lean_obj_res cleanode_get_unix_time(lean_obj_arg world) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)tv.tv_sec));
}

// Get current Unix time in milliseconds (for sub-slot precision)
lean_obj_res cleanode_get_unix_time_ms(lean_obj_arg world) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    uint64_t ms = (uint64_t)tv.tv_sec * 1000 + (uint64_t)tv.tv_usec / 1000;
    return lean_io_result_mk_ok(lean_box_uint64(ms));
}
