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
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <arpa/inet.h>
  #include <netdb.h>
  #define CLOSESOCKET close
  static void ensure_wsa_init(void) {}
  static const char* sock_strerror(void) { return strerror(errno); }
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
 * cleanode_socket_connect : String -> UInt16 -> IO (Except SocketError Socket)
 */
lean_obj_res cleanode_socket_connect(lean_obj_arg host_obj, uint16_t port, lean_obj_arg world) {
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
 * cleanode_socket_send : Socket -> ByteArray -> IO (Except SocketError Unit)
 */
lean_obj_res cleanode_socket_send(lean_obj_arg sock_obj, lean_obj_arg data_obj, lean_obj_arg world) {
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
 * cleanode_socket_receive : Socket -> UInt32 -> IO (Except SocketError ByteArray)
 */
lean_obj_res cleanode_socket_receive(lean_obj_arg sock_obj, uint32_t max_bytes, lean_obj_arg world) {
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
 * cleanode_socket_receive_exact : Socket -> UInt32 -> IO (Except SocketError ByteArray)
 */
lean_obj_res cleanode_socket_receive_exact(lean_obj_arg sock_obj, uint32_t num_bytes, lean_obj_arg world) {
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
 * Close socket
 * cleanode_socket_close : Socket -> IO Unit
 */
lean_obj_res cleanode_socket_close(lean_obj_arg sock_obj, lean_obj_arg world) {
    int sockfd = (int)socket_get_fd(sock_obj);
    CLOSESOCKET(sockfd);
    return lean_io_result_mk_ok(lean_box(0));
}
