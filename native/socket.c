/*
 * TCP Socket FFI for Cleanode
 *
 * Provides socket functionality to Lean 4 via FFI
 * Supports both POSIX (Linux/macOS) and WinSock2 (Windows)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <signal.h>

#ifdef _WIN32
  #include <winsock2.h>
  #include <ws2tcpip.h>
  #pragma comment(lib, "ws2_32.lib")
  typedef int ssize_t;
  #define CLOSESOCKET closesocket
  static int wsa_initialized = 0;
  static void ensure_wsa_init(void) {
      if (!wsa_initialized) {
          WSADATA wsaData;
          WSAStartup(MAKEWORD(2, 2), &wsaData);
          wsa_initialized = 1;
      }
  }
  static const char* sock_strerror(void) {
      static char buf[128];
      int err = WSAGetLastError();
      snprintf(buf, sizeof(buf), "WinSock error %d", err);
      return buf;
  }
#else
  #include <unistd.h>
  #include <poll.h>
  #include <sys/socket.h>
  #include <sys/un.h>
  #include <netinet/in.h>
  #include <arpa/inet.h>
  #include <netdb.h>
  #define CLOSESOCKET close
  static void ensure_wsa_init(void) {}
  static const char* sock_strerror(void) { return strerror(errno); }
  static int sigpipe_ignored = 0;
  static void ensure_sigpipe_ignored(void) {
      if (!sigpipe_ignored) {
          signal(SIGPIPE, SIG_IGN);
          sigpipe_ignored = 1;
      }
  }
#endif

#include <lean/lean.h>

// Socket error constructors
static inline lean_obj_res mk_socket_error_connection_failed(lean_obj_arg reason) {
    lean_object* err = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(err, 0, reason);
    return err;
}

static inline lean_obj_res mk_socket_error_send_failed(lean_obj_arg reason) {
    lean_object* err = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(err, 0, reason);
    return err;
}

static inline lean_obj_res mk_socket_error_receive_failed(lean_obj_arg reason) {
    lean_object* err = lean_alloc_ctor(2, 1, 0);
    lean_ctor_set(err, 0, reason);
    return err;
}

static inline lean_obj_res mk_except_error(lean_obj_arg err) {
    lean_object* result = lean_alloc_ctor(0, 1, 0); // Except.error is constructor 0
    lean_ctor_set(result, 0, err);
    return result;
}

static inline lean_obj_res mk_except_ok(lean_obj_arg val) {
    lean_object* result = lean_alloc_ctor(1, 1, 0); // Except.ok is constructor 1
    lean_ctor_set(result, 0, val);
    return result;
}

// Socket finalizer (called when Socket object is garbage collected)
static void socket_finalizer(void* ptr) {
    // File descriptor is stored directly, no need to close here
    // (user should call socket_close explicitly)
}

// Socket external class definition
static lean_external_class* g_socket_class = NULL;

static lean_external_class* get_socket_class() {
    if (g_socket_class == NULL) {
        g_socket_class = lean_register_external_class(socket_finalizer, NULL);
    }
    return g_socket_class;
}

// Create Socket as external object
static inline lean_obj_res mk_socket(uint32_t fd) {
    uint32_t* fd_ptr = malloc(sizeof(uint32_t));
    *fd_ptr = fd;
    return lean_alloc_external(get_socket_class(), fd_ptr);
}

static inline uint32_t socket_get_fd(lean_obj_arg sock) {
    uint32_t* fd_ptr = (uint32_t*)lean_get_external_data(sock);
    return *fd_ptr;
}

/*
 * Connect to remote host
 * dion_socket_connect : String -> UInt16 -> IO (Except SocketError Socket)
 */
lean_obj_res dion_socket_connect(lean_obj_arg host_obj, uint16_t port, lean_obj_arg world) {
    ensure_wsa_init();
    const char* host = lean_string_cstr(host_obj);

    // Resolve hostname
    struct addrinfo hints, *result, *rp;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;    // IPv4 or IPv6
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    char port_str[6];
    snprintf(port_str, sizeof(port_str), "%u", port);

    int status = getaddrinfo(host, port_str, &hints, &result);
    if (status != 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "getaddrinfo: %s", gai_strerror(status));
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        lean_object* except_err = mk_except_error(err);
        return lean_io_result_mk_ok(except_err);
    }

    // Try each address until we successfully connect
    int sockfd = -1;
    for (rp = result; rp != NULL; rp = rp->ai_next) {
        sockfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sockfd == -1) {
            continue;
        }

        if (connect(sockfd, rp->ai_addr, rp->ai_addrlen) == 0) {
            break; // Success
        }

        CLOSESOCKET(sockfd);
        sockfd = -1;
    }

    freeaddrinfo(result);

    if (sockfd == -1) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "Could not connect to %s:%u: %s",
                 host, port, sock_strerror());
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        lean_object* except_err = mk_except_error(err);
        return lean_io_result_mk_ok(except_err);
    }

    // No receive timeout — block indefinitely waiting for data.
    // Cardano blocks arrive ~20s apart, and KeepAlive pings keep the connection alive.
    // The user can Ctrl+C to stop.

    // Success!
    lean_object* socket = mk_socket((uint32_t)sockfd);
    lean_object* except_ok = mk_except_ok(socket);
    return lean_io_result_mk_ok(except_ok);
}

/*
 * Send data over socket
 * dion_socket_send : Socket -> ByteArray -> IO (Except SocketError Unit)
 */
lean_obj_res dion_socket_send(lean_obj_arg sock_obj, lean_obj_arg data_obj, lean_obj_arg world) {
#ifndef _WIN32
    ensure_sigpipe_ignored();
#endif
    int sockfd = (int)socket_get_fd(sock_obj);

    size_t len = lean_sarray_size(data_obj);
    const uint8_t* data = lean_sarray_cptr(data_obj);

    ssize_t sent = send(sockfd, data, len, 0);

    if (sent < 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "send: %s", sock_strerror());
        lean_object* err = mk_socket_error_send_failed(lean_mk_string(err_msg));
        lean_object* except_err = mk_except_error(err);
        return lean_io_result_mk_ok(except_err);
    }

    lean_object* unit = lean_box(0);
    lean_object* except_ok = mk_except_ok(unit);
    return lean_io_result_mk_ok(except_ok);
}

/*
 * Receive UP TO max_bytes from socket (single recv call)
 * dion_socket_receive : Socket -> UInt32 -> IO (Except SocketError ByteArray)
 */
lean_obj_res dion_socket_receive(lean_obj_arg sock_obj, uint32_t max_bytes, lean_obj_arg world) {
    int sockfd = (int)socket_get_fd(sock_obj);

    uint8_t* buffer = malloc(max_bytes);
    if (!buffer) {
        lean_object* err = mk_socket_error_receive_failed(lean_mk_string("malloc failed"));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    ssize_t received = recv(sockfd, buffer, max_bytes, 0);

    if (received < 0) {
        free(buffer);
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "recv: %s", sock_strerror());
        lean_object* err = mk_socket_error_receive_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    if (received == 0) {
        free(buffer);
        lean_object* err = mk_socket_error_receive_failed(lean_mk_string("connection closed by peer"));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    lean_object* byte_array = lean_alloc_sarray(sizeof(uint8_t), received, received);
    memcpy(lean_sarray_cptr(byte_array), buffer, received);
    free(buffer);
    return lean_io_result_mk_ok(mk_except_ok(byte_array));
}

/*
 * Receive EXACTLY num_bytes from socket (loops until all received)
 * dion_socket_receive_exact : Socket -> UInt32 -> IO (Except SocketError ByteArray)
 */
lean_obj_res dion_socket_receive_exact(lean_obj_arg sock_obj, uint32_t num_bytes, lean_obj_arg world) {
    int sockfd = (int)socket_get_fd(sock_obj);

    uint8_t* buffer = malloc(num_bytes);
    if (!buffer) {
        lean_object* err = mk_socket_error_receive_failed(lean_mk_string("malloc failed"));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    size_t total_received = 0;
    while (total_received < num_bytes) {
        ssize_t received = recv(sockfd, buffer + total_received, num_bytes - total_received, 0);

        if (received < 0) {
            free(buffer);
            char err_msg[256];
            snprintf(err_msg, sizeof(err_msg), "recv: %s", sock_strerror());
            lean_object* err = mk_socket_error_receive_failed(lean_mk_string(err_msg));
            return lean_io_result_mk_ok(mk_except_error(err));
        }

        if (received == 0) {
            free(buffer);
            lean_object* err = mk_socket_error_receive_failed(lean_mk_string("connection closed by peer"));
            return lean_io_result_mk_ok(mk_except_error(err));
        }

        total_received += received;
    }

    lean_object* byte_array = lean_alloc_sarray(sizeof(uint8_t), total_received, total_received);
    memcpy(lean_sarray_cptr(byte_array), buffer, total_received);
    free(buffer);
    return lean_io_result_mk_ok(mk_except_ok(byte_array));
}

/*
 * Receive exactly num_bytes with a timeout in milliseconds.
 * Returns Except SocketError (Option ByteArray):
 *   Ok (some bytes) — received all bytes
 *   Ok none         — timed out (no data within timeout_ms)
 *   Error e         — socket error or connection closed
 *
 * dion_socket_receive_exact_timeout : Socket -> UInt32 -> UInt32 -> IO (Except SocketError (Option ByteArray))
 */
lean_obj_res dion_socket_receive_exact_timeout(lean_obj_arg sock_obj, uint32_t num_bytes, uint32_t timeout_ms, lean_obj_arg world) {
    int sockfd = (int)socket_get_fd(sock_obj);

    uint8_t* buffer = malloc(num_bytes);
    if (!buffer) {
        lean_object* err = mk_socket_error_receive_failed(lean_mk_string("malloc failed"));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    size_t total_received = 0;
    while (total_received < num_bytes) {
        /* Use poll() to wait with timeout before each recv */
        struct pollfd pfd;
        pfd.fd = sockfd;
        pfd.events = POLLIN;
        pfd.revents = 0;

        /* For the first chunk, use the full timeout. For subsequent chunks
           (partial read), use a shorter timeout since data should be arriving. */
        int poll_timeout = (total_received == 0) ? (int)timeout_ms : 5000;
        int poll_ret = poll(&pfd, 1, poll_timeout);

        if (poll_ret == 0) {
            /* Timeout */
            free(buffer);
            if (total_received == 0) {
                /* Clean timeout — no data at all, return None */
                /* Option.none = ctor 0, no fields */
                return lean_io_result_mk_ok(mk_except_ok(lean_box(0)));
            } else {
                /* Partial read timeout — treat as error */
                lean_object* err = mk_socket_error_receive_failed(lean_mk_string("timeout during partial read"));
                return lean_io_result_mk_ok(mk_except_error(err));
            }
        }
        if (poll_ret < 0) {
            free(buffer);
            char err_msg[256];
            snprintf(err_msg, sizeof(err_msg), "poll: %s", sock_strerror());
            lean_object* err = mk_socket_error_receive_failed(lean_mk_string(err_msg));
            return lean_io_result_mk_ok(mk_except_error(err));
        }

        ssize_t received = recv(sockfd, buffer + total_received, num_bytes - total_received, 0);
        if (received < 0) {
            free(buffer);
            char err_msg[256];
            snprintf(err_msg, sizeof(err_msg), "recv: %s", sock_strerror());
            lean_object* err = mk_socket_error_receive_failed(lean_mk_string(err_msg));
            return lean_io_result_mk_ok(mk_except_error(err));
        }
        if (received == 0) {
            free(buffer);
            lean_object* err = mk_socket_error_receive_failed(lean_mk_string("connection closed by peer"));
            return lean_io_result_mk_ok(mk_except_error(err));
        }
        total_received += received;
    }

    lean_object* byte_array = lean_alloc_sarray(sizeof(uint8_t), total_received, total_received);
    memcpy(lean_sarray_cptr(byte_array), buffer, total_received);
    free(buffer);
    /* Option.some byte_array = ctor 1, 1 field */
    lean_object* some_val = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(some_val, 0, byte_array);
    return lean_io_result_mk_ok(mk_except_ok(some_val));
}

/*
 * Resolve hostname to all IP addresses (DNS discovery)
 * dion_dns_resolve : String -> IO (Array String)
 */
lean_obj_res dion_dns_resolve(lean_obj_arg host_obj, lean_obj_arg world) {
    ensure_wsa_init();
    const char* host = lean_string_cstr(host_obj);

    struct addrinfo hints, *result, *rp;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;       // IPv4 only for simplicity
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    int status = getaddrinfo(host, NULL, &hints, &result);
    if (status != 0) {
        // Return empty array on failure
        lean_object* arr = lean_alloc_array(0, 0);
        return lean_io_result_mk_ok(arr);
    }

    // Count unique addresses
    char addrs[64][INET_ADDRSTRLEN];
    int count = 0;
    for (rp = result; rp != NULL && count < 64; rp = rp->ai_next) {
        struct sockaddr_in* addr = (struct sockaddr_in*)rp->ai_addr;
        const char* ip = inet_ntoa(addr->sin_addr);
        // Check for duplicates
        int dup = 0;
        for (int i = 0; i < count; i++) {
            if (strcmp(addrs[i], ip) == 0) { dup = 1; break; }
        }
        if (!dup) {
            strncpy(addrs[count], ip, INET_ADDRSTRLEN);
            count++;
        }
    }
    freeaddrinfo(result);

    lean_object* arr = lean_alloc_array(count, count);
    for (int i = 0; i < count; i++) {
        lean_array_set_core(arr, i, lean_mk_string(addrs[i]));
    }
    return lean_io_result_mk_ok(arr);
}


/*
 * Create a listening socket on the given port (dual-stack IPv4+IPv6)
 * dion_socket_listen : UInt16 -> IO (Except SocketError Socket)
 */
lean_obj_res dion_socket_listen(uint16_t port, lean_obj_arg world) {
    ensure_wsa_init();

    int sockfd = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP);
    if (sockfd == -1) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "socket: %s", sock_strerror());
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    // Allow port reuse
    int optval = 1;
    setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (const char*)&optval, sizeof(optval));

    // Accept both IPv4 and IPv6 connections
    int v6only = 0;
    setsockopt(sockfd, IPPROTO_IPV6, IPV6_V6ONLY, (const char*)&v6only, sizeof(v6only));

    struct sockaddr_in6 addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin6_family = AF_INET6;
    addr.sin6_port = htons(port);
    addr.sin6_addr = in6addr_any;

    if (bind(sockfd, (struct sockaddr*)&addr, sizeof(addr)) != 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "bind port %u: %s", port, sock_strerror());
        CLOSESOCKET(sockfd);
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    if (listen(sockfd, 128) != 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "listen: %s", sock_strerror());
        CLOSESOCKET(sockfd);
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    lean_object* socket = mk_socket((uint32_t)sockfd);
    return lean_io_result_mk_ok(mk_except_ok(socket));
}

/*
 * Accept one connection from a listening socket
 * dion_socket_accept : Socket -> IO (Except SocketError Socket)
 */
lean_obj_res dion_socket_accept(lean_obj_arg sock_obj, lean_obj_arg world) {
    int listenfd = (int)socket_get_fd(sock_obj);

    struct sockaddr_in6 client_addr;
    socklen_t addr_len = sizeof(client_addr);
    int clientfd = accept(listenfd, (struct sockaddr*)&client_addr, &addr_len);
    if (clientfd == -1) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "accept: %s", sock_strerror());
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    lean_object* socket = mk_socket((uint32_t)clientfd);
    return lean_io_result_mk_ok(mk_except_ok(socket));
}

/*
 * Close socket
 * dion_socket_close : Socket -> IO Unit
 */
lean_obj_res dion_socket_close(lean_obj_arg sock_obj, lean_obj_arg world) {
    int sockfd = (int)socket_get_fd(sock_obj);
    CLOSESOCKET(sockfd);
    return lean_io_result_mk_ok(lean_box(0));
}

#ifndef _WIN32
/*
 * Create a listening Unix domain socket at the given path.
 * Unlinks any stale socket file before binding.
 * dion_unix_listen : String -> IO (Except SocketError Socket)
 */
lean_obj_res dion_unix_listen(lean_obj_arg path_obj, lean_obj_arg world) {
    const char* path = lean_string_cstr(path_obj);

    int sockfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sockfd == -1) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "unix socket: %s", sock_strerror());
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    // Remove stale socket file if it exists
    unlink(path);

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    // Ensure path fits (sun_path is typically 104 or 108 bytes)
    if (strlen(path) >= sizeof(addr.sun_path)) {
        CLOSESOCKET(sockfd);
        lean_object* err = mk_socket_error_connection_failed(
            lean_mk_string("unix socket path too long"));
        return lean_io_result_mk_ok(mk_except_error(err));
    }
    strncpy(addr.sun_path, path, sizeof(addr.sun_path) - 1);

    if (bind(sockfd, (struct sockaddr*)&addr, sizeof(addr)) != 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "unix bind %s: %s", path, sock_strerror());
        CLOSESOCKET(sockfd);
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    if (listen(sockfd, 5) != 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "unix listen: %s", sock_strerror());
        CLOSESOCKET(sockfd);
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    lean_object* socket = mk_socket((uint32_t)sockfd);
    return lean_io_result_mk_ok(mk_except_ok(socket));
}

/*
 * Accept one connection from a Unix domain listening socket.
 * dion_unix_accept : Socket -> IO (Except SocketError Socket)
 */
lean_obj_res dion_unix_accept(lean_obj_arg sock_obj, lean_obj_arg world) {
    int listenfd = (int)socket_get_fd(sock_obj);

    struct sockaddr_un client_addr;
    socklen_t addr_len = sizeof(client_addr);
    int clientfd = accept(listenfd, (struct sockaddr*)&client_addr, &addr_len);
    if (clientfd == -1) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "unix accept: %s", sock_strerror());
        lean_object* err = mk_socket_error_connection_failed(lean_mk_string(err_msg));
        return lean_io_result_mk_ok(mk_except_error(err));
    }

    lean_object* socket = mk_socket((uint32_t)clientfd);
    return lean_io_result_mk_ok(mk_except_ok(socket));
}

/*
 * Close a Unix socket and unlink the socket file.
 * dion_unix_close_and_unlink : Socket -> String -> IO Unit
 */
lean_obj_res dion_unix_close_and_unlink(lean_obj_arg sock_obj, lean_obj_arg path_obj, lean_obj_arg world) {
    int sockfd = (int)socket_get_fd(sock_obj);
    const char* path = lean_string_cstr(path_obj);
    CLOSESOCKET(sockfd);
    unlink(path);
    return lean_io_result_mk_ok(lean_box(0));
}
#endif /* !_WIN32 */
