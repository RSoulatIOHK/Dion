/*
 * Compatibility shim for glibc symbol versioning.
 *
 * System gcc on Ubuntu 22.04+ emits references to fcntl64 (glibc 2.28+),
 * but Lean's bundled sysroot has an older glibc without it.
 * This provides fcntl64 as a thin wrapper around fcntl.
 */

#include <fcntl.h>
#include <stdarg.h>

int fcntl64(int fd, int cmd, ...) {
    va_list ap;
    va_start(ap, cmd);
    void* arg = va_arg(ap, void*);
    va_end(ap);
    return fcntl(fd, cmd, arg);
}
