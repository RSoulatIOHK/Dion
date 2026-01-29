#include <lean/lean.h>

// Simple test function that returns an integer
lean_obj_res cleanode_test_simple(uint32_t x, lean_obj_arg world) {
    uint32_t result = x + 42;
    lean_object* res = lean_box_uint32(result);
    return lean_io_result_mk_ok(res);
}
