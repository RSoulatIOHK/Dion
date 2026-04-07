/*
 * Minimal HTTP Server FFI for Cleanode Prometheus Metrics
 *
 * Single-threaded POSIX socket server that serves GET /metrics only.
 * Designed to run in a background thread; the Lean side spawns it via Task.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <lean/lean.h>

/* ========================
 * Lean FFI helpers
 * ======================== */

static lean_obj_res mk_except_ok_srv(lean_obj_arg val) {
    lean_object *r = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(r, 0, val);
    return r;
}

static lean_obj_res mk_except_error_srv(lean_obj_arg val) {
    lean_object *r = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(r, 0, val);
    return r;
}

/* ========================
 * Metrics callback
 * ======================== */

/*
 * Global callback: the Lean side sets this to a closure that returns the
 * current Prometheus metrics text. We call it on each request.
 */
static lean_object *g_metrics_callback = NULL;

/*
 * Set the metrics callback. Called once from Lean before starting the server.
 * The callback has type: Unit → IO String
 */
LEAN_EXPORT lean_obj_res dion_metrics_set_callback(lean_obj_arg callback, lean_obj_arg world) {
    if (g_metrics_callback) {
        lean_dec_ref(g_metrics_callback);
    }
    g_metrics_callback = callback;
    /* Don't dec_ref — we keep ownership */
    return lean_io_result_mk_ok(lean_box(0));
}

/* ========================
 * HTTP Server
 * ======================== */

static void send_response(int client_fd, int status, const char *status_text,
                          const char *content_type, const char *body, size_t body_len) {
    char header[512];
    int hlen = snprintf(header, sizeof(header),
        "HTTP/1.1 %d %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %zu\r\n"
        "Connection: close\r\n"
        "\r\n",
        status, status_text, content_type, body_len);
    write(client_fd, header, hlen);
    if (body_len > 0) {
        write(client_fd, body, body_len);
    }
}

static void handle_client(int client_fd) {
    char buf[4096];
    ssize_t n = read(client_fd, buf, sizeof(buf) - 1);
    if (n <= 0) { close(client_fd); return; }
    buf[n] = '\0';

    /* Only handle GET /metrics */
    if (strncmp(buf, "GET /metrics", 12) == 0) {
        if (g_metrics_callback) {
            /* Call the Lean callback: callback () world */
            lean_inc_ref(g_metrics_callback);
            lean_object *unit_val = lean_box(0);
            lean_object *io_result = lean_apply_2(g_metrics_callback, unit_val, lean_io_mk_world());

            if (lean_io_result_is_ok(io_result)) {
                lean_object *str_obj = lean_io_result_get_value(io_result);
                lean_inc_ref(str_obj);
                const char *metrics = lean_string_cstr(str_obj);
                size_t len = strlen(metrics);
                send_response(client_fd, 200, "OK",
                    "text/plain; version=0.0.4; charset=utf-8", metrics, len);
                lean_dec_ref(str_obj);
            } else {
                const char *err = "# Error generating metrics\n";
                send_response(client_fd, 500, "Internal Server Error",
                    "text/plain", err, strlen(err));
            }
            lean_dec_ref(io_result);
        } else {
            const char *err = "# No metrics callback registered\n";
            send_response(client_fd, 503, "Service Unavailable",
                "text/plain", err, strlen(err));
        }
    } else if (strncmp(buf, "GET / ", 6) == 0 || strncmp(buf, "GET / HTTP", 10) == 0) {
        const char *body = "Cleanode Metrics Server\nGET /metrics for Prometheus\n";
        send_response(client_fd, 200, "OK", "text/plain", body, strlen(body));
    } else {
        send_response(client_fd, 404, "Not Found", "text/plain", "Not Found\n", 10);
    }

    close(client_fd);
}

/*
 * Start the HTTP metrics server on the given port.
 * This blocks forever (call from a background Task).
 *
 * @param port  Port number to listen on
 * @return IO (Except String Unit) — only returns on error
 */
LEAN_EXPORT lean_obj_res dion_metrics_server_start(uint16_t port, lean_obj_arg world) {
    int server_fd = socket(AF_INET6, SOCK_STREAM, 0);
    if (server_fd < 0) {
        return lean_io_result_mk_ok(mk_except_error_srv(lean_mk_string("Failed to create socket")));
    }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    /* Dual-stack: accept both IPv4 and IPv6 */
    int off = 0;
    setsockopt(server_fd, IPPROTO_IPV6, IPV6_V6ONLY, &off, sizeof(off));

    struct sockaddr_in6 addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin6_family = AF_INET6;
    addr.sin6_port = htons(port);
    addr.sin6_addr = in6addr_any;

    if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        close(server_fd);
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "Failed to bind to port %d", port);
        return lean_io_result_mk_ok(mk_except_error_srv(lean_mk_string(errbuf)));
    }

    if (listen(server_fd, 8) < 0) {
        close(server_fd);
        return lean_io_result_mk_ok(mk_except_error_srv(lean_mk_string("Failed to listen")));
    }

    /* Accept loop — runs forever */
    while (1) {
        int client_fd = accept(server_fd, NULL, NULL);
        if (client_fd < 0) continue;
        handle_client(client_fd);
    }

    /* Unreachable */
    close(server_fd);
    return lean_io_result_mk_ok(mk_except_ok_srv(lean_box(0)));
}
