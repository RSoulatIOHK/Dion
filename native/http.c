/*
 * HTTP Client FFI for Cleanode
 *
 * Wraps libcurl to provide HTTPS GET and file download capabilities.
 * Used by the Mithril integration for snapshot discovery and download.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
#include <lean/lean.h>

/* ========================
 * Response buffer
 * ======================== */

typedef struct {
    char *data;
    size_t size;
    size_t capacity;
} ResponseBuffer;

static ResponseBuffer *response_buffer_new(void) {
    ResponseBuffer *buf = (ResponseBuffer *)malloc(sizeof(ResponseBuffer));
    buf->data = (char *)malloc(4096);
    buf->size = 0;
    buf->capacity = 4096;
    return buf;
}

static void response_buffer_free(ResponseBuffer *buf) {
    if (buf) {
        free(buf->data);
        free(buf);
    }
}

static size_t write_callback(char *ptr, size_t size, size_t nmemb, void *userdata) {
    ResponseBuffer *buf = (ResponseBuffer *)userdata;
    size_t total = size * nmemb;

    while (buf->size + total >= buf->capacity) {
        buf->capacity *= 2;
        buf->data = (char *)realloc(buf->data, buf->capacity);
        if (!buf->data) return 0;
    }

    memcpy(buf->data + buf->size, ptr, total);
    buf->size += total;
    return total;
}

/* ========================
 * Lean FFI helpers
 * ======================== */

/* Make IO.Error from a string */
static lean_obj_res mk_io_error(const char *msg) {
    return lean_mk_io_user_error(lean_mk_string(msg));
}

/* Make Except.ok value */
static lean_obj_res mk_except_ok(lean_obj_arg val) {
    lean_object *r = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(r, 0, val);
    return r;
}

/* Make Except.error value */
static lean_obj_res mk_except_error(lean_obj_arg val) {
    lean_object *r = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(r, 0, val);
    return r;
}

/* ========================
 * HTTP GET (returns ByteArray)
 * ======================== */

LEAN_EXPORT lean_obj_res cleanode_http_get(b_lean_obj_arg url_obj, lean_obj_arg world) {
    const char *url = lean_string_cstr(url_obj);
    CURL *curl = curl_easy_init();
    if (!curl) {
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string("Failed to initialize curl")));
    }

    ResponseBuffer *buf = response_buffer_new();

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, buf);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "cleanode/0.1");

    CURLcode res = curl_easy_perform(curl);

    if (res != CURLE_OK) {
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "HTTP request failed: %s", curl_easy_strerror(res));
        response_buffer_free(buf);
        curl_easy_cleanup(curl);
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string(errbuf)));
    }

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    if (http_code >= 400) {
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "HTTP %ld error", http_code);
        response_buffer_free(buf);
        curl_easy_cleanup(curl);
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string(errbuf)));
    }

    /* Convert response to Lean ByteArray */
    lean_object *result = lean_alloc_sarray(1, buf->size, buf->size);
    memcpy(lean_sarray_cptr(result), buf->data, buf->size);

    response_buffer_free(buf);
    curl_easy_cleanup(curl);

    return lean_io_result_mk_ok(mk_except_ok(result));
}

/* ========================
 * HTTP GET JSON (returns String)
 * ======================== */

LEAN_EXPORT lean_obj_res cleanode_http_get_json(b_lean_obj_arg url_obj, lean_obj_arg world) {
    const char *url = lean_string_cstr(url_obj);
    CURL *curl = curl_easy_init();
    if (!curl) {
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string("Failed to initialize curl")));
    }

    ResponseBuffer *buf = response_buffer_new();

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Accept: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, buf);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "cleanode/0.1");

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);

    if (res != CURLE_OK) {
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "HTTP request failed: %s", curl_easy_strerror(res));
        response_buffer_free(buf);
        curl_easy_cleanup(curl);
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string(errbuf)));
    }

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    if (http_code >= 400) {
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "HTTP %ld error", http_code);
        response_buffer_free(buf);
        curl_easy_cleanup(curl);
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string(errbuf)));
    }

    /* Null-terminate and convert to Lean String */
    buf->data = (char *)realloc(buf->data, buf->size + 1);
    buf->data[buf->size] = '\0';
    lean_object *result = lean_mk_string(buf->data);

    response_buffer_free(buf);
    curl_easy_cleanup(curl);

    return lean_io_result_mk_ok(mk_except_ok(result));
}

/* ========================
 * HTTP Download to File
 * ======================== */

static size_t write_file_callback(char *ptr, size_t size, size_t nmemb, void *userdata) {
    FILE *fp = (FILE *)userdata;
    return fwrite(ptr, size, nmemb, fp);
}

LEAN_EXPORT lean_obj_res cleanode_http_download(b_lean_obj_arg url_obj, b_lean_obj_arg path_obj, lean_obj_arg world) {
    const char *url = lean_string_cstr(url_obj);
    const char *path = lean_string_cstr(path_obj);

    FILE *fp = fopen(path, "wb");
    if (!fp) {
        char errbuf[512];
        snprintf(errbuf, sizeof(errbuf), "Cannot open file for writing: %s", path);
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string(errbuf)));
    }

    CURL *curl = curl_easy_init();
    if (!curl) {
        fclose(fp);
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string("Failed to initialize curl")));
    }

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_file_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 3600L);  /* 1 hour for large snapshots */
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "cleanode/0.1");

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    fclose(fp);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "Download failed: %s", curl_easy_strerror(res));
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string(errbuf)));
    }

    if (http_code >= 400) {
        char errbuf[256];
        snprintf(errbuf, sizeof(errbuf), "HTTP %ld error during download", http_code);
        return lean_io_result_mk_ok(mk_except_error(lean_mk_string(errbuf)));
    }

    /* Return Except.ok Unit */
    return lean_io_result_mk_ok(mk_except_ok(lean_box(0)));
}
