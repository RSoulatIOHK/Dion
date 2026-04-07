/*
 * Terminal Raw Mode FFI for Cleanode TUI
 *
 * Provides raw terminal input (no line buffering, no echo)
 * so we can read individual keypresses for interactive TUI.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <signal.h>
#include <sys/select.h>
#include <lean/lean.h>

static struct termios orig_termios;
static int raw_mode_active = 0;
static int atexit_registered = 0;

/*
 * Restore terminal at exit (atexit handler) — ensures terminal is never
 * left in raw mode even on crash or unhandled signal.
 */
static void restore_terminal_atexit(void) {
    if (raw_mode_active) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
        /* Restore alternate screen + cursor (best effort) */
        write(STDOUT_FILENO, "\033[?1049l\033[?25h", 14);
        raw_mode_active = 0;
    }
}

/*
 * Signal handler for SIGINT/SIGTERM — restore terminal then re-raise.
 */
static void signal_handler(int sig) {
    restore_terminal_atexit();
    signal(sig, SIG_DFL);
    raise(sig);
}

/*
 * Enable raw mode: disable canonical mode + echo, keep ISIG for Ctrl+C.
 * Returns 0 on failure, 1 on success.
 */
lean_obj_res dion_terminal_enable_raw(lean_obj_arg world) {
    if (raw_mode_active) {
        return lean_io_result_mk_ok(lean_box(1));
    }
    if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) {
        return lean_io_result_mk_ok(lean_box(0));
    }
    struct termios raw = orig_termios;
    /* Turn off canonical mode and echo, but keep ISIG so Ctrl+C works */
    raw.c_lflag &= ~(ICANON | ECHO);
    /* Turn off input processing */
    raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
    /* Min 0 chars, 100ms timeout */
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) {
        return lean_io_result_mk_ok(lean_box(0));
    }
    raw_mode_active = 1;
    /* Register cleanup handlers (once) */
    if (!atexit_registered) {
        atexit(restore_terminal_atexit);
        signal(SIGINT, signal_handler);
        signal(SIGTERM, signal_handler);
        atexit_registered = 1;
    }
    return lean_io_result_mk_ok(lean_box(1));
}

/*
 * Restore terminal to original mode.
 */
lean_obj_res dion_terminal_disable_raw(lean_obj_arg world) {
    if (raw_mode_active) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
        raw_mode_active = 0;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * Read a single keypress. Returns a Lean ByteArray with 0..6 bytes.
 * Empty array means no key available (timeout).
 * For escape sequences (arrows etc), returns the full sequence.
 *
 * Key mappings:
 *   Arrow Up    = ESC [ A  (3 bytes: 27, 91, 65)
 *   Arrow Down  = ESC [ B  (3 bytes: 27, 91, 66)
 *   Arrow Right = ESC [ C  (3 bytes: 27, 91, 67)
 *   Arrow Left  = ESC [ D  (3 bytes: 27, 91, 68)
 *   Enter       = 13
 *   Escape      = 27 (alone, after timeout)
 *   'q'         = 113
 *   Space       = 32
 */
lean_obj_res dion_terminal_read_key(lean_obj_arg world) {
    unsigned char buf[6];
    ssize_t n = read(STDIN_FILENO, buf, 1);
    if (n <= 0) {
        /* No input available */
        lean_obj_res arr = lean_alloc_sarray(1, 0, 0);
        return lean_io_result_mk_ok(arr);
    }

    if (buf[0] == 27) {
        /* Could be escape sequence or standalone ESC */
        /* Try to read more bytes with short timeout */
        unsigned char seq[5];
        ssize_t n2 = read(STDIN_FILENO, seq, 1);
        if (n2 <= 0) {
            /* Standalone ESC */
            lean_obj_res arr = lean_alloc_sarray(1, 1, 1);
            uint8_t *p = lean_sarray_cptr(arr);
            p[0] = 27;
            return lean_io_result_mk_ok(arr);
        }
        if (seq[0] == '[') {
            /* CSI sequence */
            ssize_t n3 = read(STDIN_FILENO, seq + 1, 1);
            if (n3 > 0) {
                /* Return 3-byte sequence: ESC [ X */
                lean_obj_res arr = lean_alloc_sarray(1, 3, 3);
                uint8_t *p = lean_sarray_cptr(arr);
                p[0] = 27;
                p[1] = '[';
                p[2] = seq[1];
                return lean_io_result_mk_ok(arr);
            }
        }
        /* ESC + something else: return 2 bytes */
        lean_obj_res arr = lean_alloc_sarray(1, 2, 2);
        uint8_t *p = lean_sarray_cptr(arr);
        p[0] = 27;
        p[1] = seq[0];
        return lean_io_result_mk_ok(arr);
    }

    /* Regular single byte */
    lean_obj_res arr = lean_alloc_sarray(1, 1, 1);
    uint8_t *p = lean_sarray_cptr(arr);
    p[0] = buf[0];
    return lean_io_result_mk_ok(arr);
}
