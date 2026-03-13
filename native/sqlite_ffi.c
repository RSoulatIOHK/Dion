/*
 * SQLite3 FFI for Cleanode
 *
 * Provides persistent block storage via SQLite3.
 * The database stores blocks, sync state, and supports indexed lookups
 * by block number, slot, and hash.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>
#include <lean/lean.h>

/* ---------- External class for sqlite3* handle ---------- */

static void db_finalizer(void* ptr) {
    if (ptr) {
        sqlite3_close((sqlite3*)ptr);
    }
}

static void db_foreach(void* ptr, b_lean_obj_arg fn) {
    (void)ptr; (void)fn;
}

static lean_external_class* g_db_class = NULL;

static lean_external_class* get_db_class(void) {
    if (g_db_class == NULL) {
        g_db_class = lean_register_external_class(db_finalizer, db_foreach);
    }
    return g_db_class;
}

static inline lean_obj_res mk_db(sqlite3* db) {
    return lean_alloc_external(get_db_class(), db);
}

static inline sqlite3* get_db_ptr(b_lean_obj_arg obj) {
    return (sqlite3*)lean_get_external_data(obj);
}

/* ---------- Helpers ---------- */

static inline lean_obj_res mk_io_ok(lean_obj_arg val) {
    return lean_io_result_mk_ok(val);
}

static inline lean_obj_res mk_io_err(const char* msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

static inline lean_obj_res mk_option_none(void) {
    return lean_box(0);  /* Option.none */
}

static inline lean_obj_res mk_option_some(lean_obj_arg val) {
    lean_object* obj = lean_alloc_ctor(1, 1, 0);  /* Option.some */
    lean_ctor_set(obj, 0, val);
    return obj;
}

/* Create a ByteArray from raw bytes */
static inline lean_obj_res mk_byte_array(const unsigned char* data, size_t len) {
    lean_object* arr = lean_alloc_sarray(1, len, len);
    if (data && len > 0) {
        memcpy(lean_sarray_cptr(arr), data, len);
    }
    return arr;
}

/* ---------- Schema ---------- */

static const char* SCHEMA_SQL =
    "CREATE TABLE IF NOT EXISTS blocks ("
    "  block_no   INTEGER NOT NULL,"
    "  slot       INTEGER NOT NULL,"
    "  era        INTEGER NOT NULL,"
    "  hash       BLOB NOT NULL,"
    "  prev_hash  BLOB NOT NULL,"
    "  header     BLOB NOT NULL,"
    "  body       BLOB,"
    "  immutable  INTEGER NOT NULL DEFAULT 0,"
    "  PRIMARY KEY (block_no)"
    ");"
    "CREATE INDEX IF NOT EXISTS idx_blocks_slot ON blocks(slot);"
    "CREATE INDEX IF NOT EXISTS idx_blocks_hash ON blocks(hash);"
    "CREATE INDEX IF NOT EXISTS idx_blocks_immutable ON blocks(immutable);"
    ""
    "CREATE TABLE IF NOT EXISTS sync_state ("
    "  id         INTEGER PRIMARY KEY CHECK (id = 1),"
    "  last_slot  INTEGER NOT NULL,"
    "  last_block INTEGER NOT NULL,"
    "  last_hash  BLOB NOT NULL"
    ");"
    ""
    "CREATE TABLE IF NOT EXISTS metadata ("
    "  key   TEXT PRIMARY KEY,"
    "  value TEXT NOT NULL"
    ");";

/* ---------- FFI Functions ---------- */

/**
 * Open (or create) a SQLite database at the given path.
 * Creates the schema if the database is new.
 *
 * cleanode_db_open : String -> IO Database
 */
lean_obj_res cleanode_db_open(b_lean_obj_arg path, lean_obj_arg world) {
    (void)world;
    const char* db_path = lean_string_cstr(path);
    sqlite3* db = NULL;

    int rc = sqlite3_open(db_path, &db);
    if (rc != SQLITE_OK) {
        const char* err = sqlite3_errmsg(db);
        char buf[512];
        snprintf(buf, sizeof(buf), "Failed to open database '%s': %s", db_path, err);
        sqlite3_close(db);
        return mk_io_err(buf);
    }

    /* Enable WAL mode for better concurrent read performance */
    sqlite3_exec(db, "PRAGMA journal_mode=WAL;", NULL, NULL, NULL);
    /* Enable foreign keys */
    sqlite3_exec(db, "PRAGMA foreign_keys=ON;", NULL, NULL, NULL);
    /* Reasonable busy timeout (5 seconds) */
    sqlite3_busy_timeout(db, 5000);

    /* Create schema */
    char* errmsg = NULL;
    rc = sqlite3_exec(db, SCHEMA_SQL, NULL, NULL, &errmsg);
    if (rc != SQLITE_OK) {
        char buf[512];
        snprintf(buf, sizeof(buf), "Failed to create schema: %s", errmsg ? errmsg : "unknown");
        sqlite3_free(errmsg);
        sqlite3_close(db);
        return mk_io_err(buf);
    }

    return mk_io_ok(mk_db(db));
}

/**
 * Close the database.
 *
 * cleanode_db_close : Database -> IO Unit
 */
lean_obj_res cleanode_db_close(b_lean_obj_arg db_obj, lean_obj_arg world) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);
    if (db) {
        sqlite3_close(db);
        /* Note: finalizer will also try to close, but double-close is safe
           since we set the external data to the same pointer */
    }
    return mk_io_ok(lean_box(0));
}

/**
 * Store a block in the database.
 *
 * cleanode_db_put_block : Database -> Nat -> Nat -> Nat -> ByteArray -> ByteArray
 *                       -> ByteArray -> Bool -> IO Unit
 */
lean_obj_res cleanode_db_put_block(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg block_no_obj,
    b_lean_obj_arg slot_obj,
    b_lean_obj_arg era_obj,
    b_lean_obj_arg hash_obj,
    b_lean_obj_arg prev_hash_obj,
    b_lean_obj_arg header_obj,
    uint8_t immutable,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);

    size_t block_no = lean_unbox(block_no_obj);
    size_t slot     = lean_unbox(slot_obj);
    size_t era      = lean_unbox(era_obj);

    const unsigned char* hash_data = lean_sarray_cptr(hash_obj);
    size_t hash_len = lean_sarray_size(hash_obj);

    const unsigned char* prev_hash_data = lean_sarray_cptr(prev_hash_obj);
    size_t prev_hash_len = lean_sarray_size(prev_hash_obj);

    const unsigned char* header_data = lean_sarray_cptr(header_obj);
    size_t header_len = lean_sarray_size(header_obj);

    const char* sql =
        "INSERT OR REPLACE INTO blocks (block_no, slot, era, hash, prev_hash, header, immutable) "
        "VALUES (?, ?, ?, ?, ?, ?, ?)";

    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    sqlite3_bind_int64(stmt, 1, (sqlite3_int64)block_no);
    sqlite3_bind_int64(stmt, 2, (sqlite3_int64)slot);
    sqlite3_bind_int64(stmt, 3, (sqlite3_int64)era);
    sqlite3_bind_blob(stmt, 4, hash_data, (int)hash_len, SQLITE_STATIC);
    sqlite3_bind_blob(stmt, 5, prev_hash_data, (int)prev_hash_len, SQLITE_STATIC);
    sqlite3_bind_blob(stmt, 6, header_data, (int)header_len, SQLITE_STATIC);
    sqlite3_bind_int(stmt, 7, immutable ? 1 : 0);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    return mk_io_ok(lean_box(0));
}

/**
 * Store block body data separately (for large payloads fetched via BlockFetch).
 *
 * cleanode_db_put_block_body : Database -> Nat -> ByteArray -> IO Unit
 */
lean_obj_res cleanode_db_put_block_body(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg block_no_obj,
    b_lean_obj_arg body_obj,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);
    size_t block_no = lean_unbox(block_no_obj);

    const unsigned char* body_data = lean_sarray_cptr(body_obj);
    size_t body_len = lean_sarray_size(body_obj);

    const char* sql = "UPDATE blocks SET body = ? WHERE block_no = ?";
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    sqlite3_bind_blob(stmt, 1, body_data, (int)body_len, SQLITE_STATIC);
    sqlite3_bind_int64(stmt, 2, (sqlite3_int64)block_no);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    return mk_io_ok(lean_box(0));
}

/**
 * Retrieve a block by block number.
 * Returns: Option (blockNo, slot, era, hash, prevHash, header, body?, immutable)
 *
 * cleanode_db_get_block : Database -> Nat -> IO (Option BlockRow)
 *
 * BlockRow is a tuple: (Nat × Nat × Nat × ByteArray × ByteArray × ByteArray × Option ByteArray × Bool)
 * Encoded as nested Prod constructors.
 */
lean_obj_res cleanode_db_get_block(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg block_no_obj,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);
    size_t block_no = lean_unbox(block_no_obj);

    const char* sql =
        "SELECT block_no, slot, era, hash, prev_hash, header, body, immutable "
        "FROM blocks WHERE block_no = ?";

    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_ok(mk_option_none());
    }

    sqlite3_bind_int64(stmt, 1, (sqlite3_int64)block_no);
    rc = sqlite3_step(stmt);

    if (rc != SQLITE_ROW) {
        sqlite3_finalize(stmt);
        return mk_io_ok(mk_option_none());
    }

    /* Extract fields */
    sqlite3_int64 bn  = sqlite3_column_int64(stmt, 0);
    sqlite3_int64 sl  = sqlite3_column_int64(stmt, 1);
    sqlite3_int64 er  = sqlite3_column_int64(stmt, 2);

    const unsigned char* hash_data = sqlite3_column_blob(stmt, 3);
    int hash_len = sqlite3_column_bytes(stmt, 3);

    const unsigned char* prev_data = sqlite3_column_blob(stmt, 4);
    int prev_len = sqlite3_column_bytes(stmt, 4);

    const unsigned char* hdr_data = sqlite3_column_blob(stmt, 5);
    int hdr_len = sqlite3_column_bytes(stmt, 5);

    const unsigned char* body_data = sqlite3_column_blob(stmt, 6);
    int body_len = sqlite3_column_bytes(stmt, 6);

    int imm = sqlite3_column_int(stmt, 7);

    /* Build the result structure.
     * We return a flat structure with named fields on the Lean side,
     * so we build a constructor with 8 fields. */
    lean_object* result = lean_alloc_ctor(0, 8, 0);
    lean_ctor_set(result, 0, lean_box((size_t)bn));
    lean_ctor_set(result, 1, lean_box((size_t)sl));
    lean_ctor_set(result, 2, lean_box((size_t)er));
    lean_ctor_set(result, 3, mk_byte_array(hash_data, (size_t)hash_len));
    lean_ctor_set(result, 4, mk_byte_array(prev_data, (size_t)prev_len));
    lean_ctor_set(result, 5, mk_byte_array(hdr_data, (size_t)hdr_len));
    lean_ctor_set(result, 6, body_data
        ? mk_option_some(mk_byte_array(body_data, (size_t)body_len))
        : mk_option_none());
    lean_ctor_set(result, 7, lean_box(imm ? 1 : 0));

    sqlite3_finalize(stmt);
    return mk_io_ok(mk_option_some(result));
}

/**
 * Retrieve a block by hash.
 *
 * cleanode_db_get_block_by_hash : Database -> ByteArray -> IO (Option BlockRow)
 */
lean_obj_res cleanode_db_get_block_by_hash(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg hash_obj,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);

    const unsigned char* hash_data = lean_sarray_cptr(hash_obj);
    size_t hash_len = lean_sarray_size(hash_obj);

    const char* sql =
        "SELECT block_no, slot, era, hash, prev_hash, header, body, immutable "
        "FROM blocks WHERE hash = ?";

    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_ok(mk_option_none());
    }

    sqlite3_bind_blob(stmt, 1, hash_data, (int)hash_len, SQLITE_STATIC);
    rc = sqlite3_step(stmt);

    if (rc != SQLITE_ROW) {
        sqlite3_finalize(stmt);
        return mk_io_ok(mk_option_none());
    }

    sqlite3_int64 bn  = sqlite3_column_int64(stmt, 0);
    sqlite3_int64 sl  = sqlite3_column_int64(stmt, 1);
    sqlite3_int64 er  = sqlite3_column_int64(stmt, 2);

    const unsigned char* h_data = sqlite3_column_blob(stmt, 3);
    int h_len = sqlite3_column_bytes(stmt, 3);
    const unsigned char* prev_data = sqlite3_column_blob(stmt, 4);
    int prev_len = sqlite3_column_bytes(stmt, 4);
    const unsigned char* hdr_data = sqlite3_column_blob(stmt, 5);
    int hdr_len = sqlite3_column_bytes(stmt, 5);
    const unsigned char* body_data = sqlite3_column_blob(stmt, 6);
    int body_len = sqlite3_column_bytes(stmt, 6);
    int imm = sqlite3_column_int(stmt, 7);

    lean_object* result = lean_alloc_ctor(0, 8, 0);
    lean_ctor_set(result, 0, lean_box((size_t)bn));
    lean_ctor_set(result, 1, lean_box((size_t)sl));
    lean_ctor_set(result, 2, lean_box((size_t)er));
    lean_ctor_set(result, 3, mk_byte_array(h_data, (size_t)h_len));
    lean_ctor_set(result, 4, mk_byte_array(prev_data, (size_t)prev_len));
    lean_ctor_set(result, 5, mk_byte_array(hdr_data, (size_t)hdr_len));
    lean_ctor_set(result, 6, body_data
        ? mk_option_some(mk_byte_array(body_data, (size_t)body_len))
        : mk_option_none());
    lean_ctor_set(result, 7, lean_box(imm ? 1 : 0));

    sqlite3_finalize(stmt);
    return mk_io_ok(mk_option_some(result));
}

/**
 * Get the tip (highest block number) in the database.
 *
 * cleanode_db_get_tip : Database -> IO (Option Nat)
 */
lean_obj_res cleanode_db_get_tip(b_lean_obj_arg db_obj, lean_obj_arg world) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);

    const char* sql = "SELECT MAX(block_no) FROM blocks";
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_ok(mk_option_none());
    }

    rc = sqlite3_step(stmt);
    if (rc != SQLITE_ROW || sqlite3_column_type(stmt, 0) == SQLITE_NULL) {
        sqlite3_finalize(stmt);
        return mk_io_ok(mk_option_none());
    }

    sqlite3_int64 tip = sqlite3_column_int64(stmt, 0);
    sqlite3_finalize(stmt);
    return mk_io_ok(mk_option_some(lean_box((size_t)tip)));
}

/**
 * Mark blocks up to a given block number as immutable.
 *
 * cleanode_db_mark_immutable : Database -> Nat -> IO Unit
 */
lean_obj_res cleanode_db_mark_immutable(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg up_to_obj,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);
    size_t up_to = lean_unbox(up_to_obj);

    const char* sql = "UPDATE blocks SET immutable = 1 WHERE block_no <= ? AND immutable = 0";
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    sqlite3_bind_int64(stmt, 1, (sqlite3_int64)up_to);
    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    return mk_io_ok(lean_box(0));
}

/**
 * Rollback: delete all volatile blocks above a given block number.
 *
 * cleanode_db_rollback : Database -> Nat -> IO Nat
 * Returns the number of blocks deleted.
 */
lean_obj_res cleanode_db_rollback(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg keep_up_to_obj,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);
    size_t keep_up_to = lean_unbox(keep_up_to_obj);

    const char* sql = "DELETE FROM blocks WHERE block_no > ? AND immutable = 0";
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    sqlite3_bind_int64(stmt, 1, (sqlite3_int64)keep_up_to);
    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    int deleted = sqlite3_changes(db);
    return mk_io_ok(lean_box((size_t)deleted));
}

/**
 * Count total blocks in the database.
 *
 * cleanode_db_count_blocks : Database -> IO Nat
 */
lean_obj_res cleanode_db_count_blocks(b_lean_obj_arg db_obj, lean_obj_arg world) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);

    const char* sql = "SELECT COUNT(*) FROM blocks";
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_ok(lean_box(0));
    }

    rc = sqlite3_step(stmt);
    sqlite3_int64 count = (rc == SQLITE_ROW) ? sqlite3_column_int64(stmt, 0) : 0;
    sqlite3_finalize(stmt);

    return mk_io_ok(lean_box((size_t)count));
}

/**
 * Save sync state (upsert).
 *
 * cleanode_db_save_sync_state : Database -> Nat -> Nat -> ByteArray -> IO Unit
 */
lean_obj_res cleanode_db_save_sync_state(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg last_slot_obj,
    b_lean_obj_arg last_block_obj,
    b_lean_obj_arg last_hash_obj,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);

    size_t last_slot  = lean_unbox(last_slot_obj);
    size_t last_block = lean_unbox(last_block_obj);
    const unsigned char* hash_data = lean_sarray_cptr(last_hash_obj);
    size_t hash_len = lean_sarray_size(last_hash_obj);

    const char* sql =
        "INSERT OR REPLACE INTO sync_state (id, last_slot, last_block, last_hash) "
        "VALUES (1, ?, ?, ?)";

    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    sqlite3_bind_int64(stmt, 1, (sqlite3_int64)last_slot);
    sqlite3_bind_int64(stmt, 2, (sqlite3_int64)last_block);
    sqlite3_bind_blob(stmt, 3, hash_data, (int)hash_len, SQLITE_STATIC);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    if (rc != SQLITE_DONE) {
        return mk_io_err(sqlite3_errmsg(db));
    }

    return mk_io_ok(lean_box(0));
}

/**
 * Load sync state.
 *
 * cleanode_db_load_sync_state : Database -> IO (Option (Nat × Nat × ByteArray))
 */
lean_obj_res cleanode_db_load_sync_state(b_lean_obj_arg db_obj, lean_obj_arg world) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);

    const char* sql = "SELECT last_slot, last_block, last_hash FROM sync_state WHERE id = 1";
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        return mk_io_ok(mk_option_none());
    }

    rc = sqlite3_step(stmt);
    if (rc != SQLITE_ROW) {
        sqlite3_finalize(stmt);
        return mk_io_ok(mk_option_none());
    }

    sqlite3_int64 last_slot  = sqlite3_column_int64(stmt, 0);
    sqlite3_int64 last_block = sqlite3_column_int64(stmt, 1);
    const unsigned char* hash_data = sqlite3_column_blob(stmt, 2);
    int hash_len = sqlite3_column_bytes(stmt, 2);

    /* Build (Nat × Nat × ByteArray) as nested Prod */
    lean_object* inner = lean_alloc_ctor(0, 2, 0);  /* Prod.mk */
    lean_ctor_set(inner, 0, lean_box((size_t)last_block));
    lean_ctor_set(inner, 1, mk_byte_array(hash_data, (size_t)hash_len));

    lean_object* result = lean_alloc_ctor(0, 2, 0);  /* Prod.mk */
    lean_ctor_set(result, 0, lean_box((size_t)last_slot));
    lean_ctor_set(result, 1, inner);

    sqlite3_finalize(stmt);
    return mk_io_ok(mk_option_some(result));
}

/**
 * Get blocks in a slot range (for chain selection).
 *
 * cleanode_db_get_blocks_in_range : Database -> Nat -> Nat -> IO (Array Nat)
 * Returns array of block numbers in the range [from_slot, to_slot].
 */
lean_obj_res cleanode_db_get_blocks_in_range(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg from_slot_obj,
    b_lean_obj_arg to_slot_obj,
    lean_obj_arg world
) {
    (void)world;
    sqlite3* db = get_db_ptr(db_obj);
    size_t from_slot = lean_unbox(from_slot_obj);
    size_t to_slot   = lean_unbox(to_slot_obj);

    const char* sql =
        "SELECT block_no FROM blocks WHERE slot >= ? AND slot <= ? ORDER BY block_no ASC";

    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        /* Return empty array */
        lean_object* arr = lean_alloc_sarray(sizeof(size_t), 0, 0);
        return mk_io_ok(arr);
    }

    sqlite3_bind_int64(stmt, 1, (sqlite3_int64)from_slot);
    sqlite3_bind_int64(stmt, 2, (sqlite3_int64)to_slot);

    /* Collect results into a Lean Array Nat */
    lean_object* arr = lean_mk_empty_array();
    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        sqlite3_int64 bn = sqlite3_column_int64(stmt, 0);
        arr = lean_array_push(arr, lean_box((size_t)bn));
    }

    sqlite3_finalize(stmt);
    return mk_io_ok(arr);
}
