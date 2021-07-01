// Copyright 2018 Beckman Coulter, Inc.
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include "osi.h"

typedef struct statement_s statement_t;

typedef struct binding_s {
  int type;
  union {
    sqlite3_int64 i;
    double d;
    char* utf8;
    void* blob;
  };
  size_t size;
} binding_t;

typedef struct bindings_s {
  int len;
  binding_t bindings[];
} bindings_t;

typedef struct bulk_s {
  int count;
  statement_t** statements;
  bindings_t** mbindings;
  const char* who;
} bulk_t;

typedef struct database_s {
  sqlite3* db;
  struct statement_s* statement;
  uv_mutex_t mutex;
  uv_cond_t cond;
  uv_thread_t thread;
  uv_async_t async;
  int busy;
  uv_thread_cb work;
  ptr callback;
  union {
    char* sql;
    sqlite3_stmt* stmt;
    bulk_t* bulk;
  };
  int sql_len;
  int sqlite_rc;
} database_t;

typedef struct statement_s {
  sqlite3_stmt* stmt;
  database_t* database;
  struct statement_s* prev;
  struct statement_s* next;
} statement_t;

typedef struct {
  uv_work_t work;
  char* filename;
  int flags;
  ptr callback;
  sqlite3* db;
  int sqlite_rc;
} open_req_t;

static void close_worker(void* arg);

static void database_worker(void* arg) {
  database_t* d = (database_t*)arg;
  uv_mutex_lock(&(d->mutex));
  for (;;) {
    uv_thread_cb work = d->work;
    if (work) {
      uv_mutex_unlock(&(d->mutex));
      (*work)(arg);
      uv_mutex_lock(&(d->mutex));
      d->work = NULL;
      uv_async_send(&(d->async));
      if ((close_worker == work) && (SQLITE_OK == d->sqlite_rc)) break;
    }
    uv_cond_wait(&(d->cond), &(d->mutex));
  }
  uv_mutex_unlock(&(d->mutex));
}

static ptr make_sqlite_error(const char* who, int rc, sqlite3* db) {
  return Scons(Sstring_to_symbol(who),
               Scons(Sinteger(TRANSLATE_SQLITE_ERRNO(rc)),
                     Sstring_utf8(sqlite3_errmsg(db), -1)));
}

static void open_worker(uv_work_t* req) {
  open_req_t* r = container_of(req, open_req_t, work);
  r->sqlite_rc = sqlite3_open_v2(r->filename, &r->db, r->flags, NULL);
}

static void async_cb(uv_async_t* handle) {
  ((uv_async_cb)handle->data)(handle);
}

static void close_free_cb(uv_handle_t* handle) {
  database_t* d = container_of(handle, database_t, async);
  free(d);
}

static void open_cb(uv_work_t* req, int status) {
  (void)status;
  open_req_t* r = container_of(req, open_req_t, work);
  ptr callback = r->callback;
  sqlite3* db = r->db;
  int sqlite_rc = r->sqlite_rc;
  free(r->filename);
  Sunlock_object(callback);
  free(r);
  if (SQLITE_OK != sqlite_rc) {
    if (db) {
      osi_add_callback1(callback, make_sqlite_error("sqlite3_open_v2", sqlite3_extended_errcode(db), db));
      sqlite3_close(db);
    } else
      osi_add_callback1(callback, osi_make_error_pair("sqlite3_open_v2", TRANSLATE_SQLITE_ERRNO(sqlite_rc)));
    return;
  }
  sqlite3_extended_result_codes(db, 1);
  database_t* d = malloc_container(database_t);
  if (!d) {
    sqlite3_close(db);
    osi_add_callback1(callback, osi_make_error_pair("osi_open_database", UV_ENOMEM));
    return;
  }
  d->db = db;
  d->statement = NULL;
  d->busy = 0;
  d->work = NULL;
  d->callback = Svoid;
  d->sql = NULL;
  d->sql_len = 0;
  d->sqlite_rc = SQLITE_OK;
  int rc = uv_async_init(osi_loop, &(d->async), async_cb);
  if (rc < 0) {
    sqlite3_close(db);
    free(d);
    osi_add_callback1(callback, osi_make_error_pair("uv_async_init", rc));
    return;
  }
  rc = uv_mutex_init(&(d->mutex));
  if (rc < 0) {
    sqlite3_close(db);
    uv_close((uv_handle_t*)&(d->async), close_free_cb);
    osi_add_callback1(callback, osi_make_error_pair("uv_mutex_init", rc));
    return;
  }
  rc = uv_cond_init(&(d->cond));
  if (rc < 0) {
    uv_mutex_destroy(&(d->mutex));
    sqlite3_close(db);
    uv_close((uv_handle_t*)&(d->async), close_free_cb);
    osi_add_callback1(callback, osi_make_error_pair("uv_cond_init", rc));
    return;
  }
  rc = uv_thread_create(&(d->thread), database_worker, d);
  if (rc < 0) {
    uv_cond_destroy(&(d->cond));
    uv_mutex_destroy(&(d->mutex));
    sqlite3_close(db);
    uv_close((uv_handle_t*)&(d->async), close_free_cb);
    osi_add_callback1(callback, osi_make_error_pair("uv_thread_create", rc));
    return;
  }
  osi_add_callback1(callback, Sunsigned((uptr)d));
}

ptr osi_open_database(const char* filename, int flags, ptr callback) {
  open_req_t* req = malloc_container(open_req_t);
  if (!req)
    return osi_make_error_pair("osi_open_database", UV_ENOMEM);
  req->filename = strdup(filename);
  req->flags = flags;
  Slock_object(callback);
  req->callback = callback;
  int rc = uv_queue_work(osi_loop, &(req->work), open_worker, open_cb);
  if (rc < 0) {
    Sunlock_object(callback);
    free(req->filename);
    free(req);
    return osi_make_error_pair("uv_queue_work", rc);
  }
  return Strue;
}

static void close_worker(void* arg) {
  database_t* d = (database_t*)arg;
  for (;;) {
    statement_t* s = d->statement;
    if (!s) break;
    d->statement = s->next;
    sqlite3_finalize(s->stmt);
    s->stmt = NULL;
    s->database = NULL;
    s->prev = NULL;
    s->next = NULL;
  }
  d->sqlite_rc = sqlite3_close(d->db);
}

static void close_cb(uv_async_t* handle) {
  database_t* d = container_of(handle, database_t, async);
  ptr callback = d->callback;
  d->busy = 0;
  d->callback = Svoid;
  Sunlock_object(callback);
  int rc = d->sqlite_rc;
  if (SQLITE_OK == rc) {
    uv_thread_join(&(d->thread));
    uv_cond_destroy(&(d->cond));
    uv_mutex_destroy(&(d->mutex));
    uv_close((uv_handle_t*)handle, close_free_cb);
    osi_add_callback1(callback, Strue);
  } else
    osi_add_callback1(callback, make_sqlite_error("sqlite3_close", rc, d->db));
}

ptr osi_close_database(uptr database, ptr callback) {
  database_t* d = (database_t*)database;
  if (d->busy)
    return osi_make_error_pair("osi_close_database", UV_EBUSY);
  d->async.data = close_cb;
  Slock_object(callback);
  uv_mutex_lock(&(d->mutex));
  d->busy = 1;
  d->work = close_worker;
  d->callback = callback;
  uv_mutex_unlock(&(d->mutex));
  uv_cond_signal(&(d->cond));
  return Strue;
}

static void prepare_worker(void* arg) {
  database_t* d = (database_t*)arg;
  sqlite3_stmt* stmt;
  d->sqlite_rc = sqlite3_prepare_v2(d->db, d->sql, d->sql_len, &stmt, NULL);
  if (SQLITE_OK == d->sqlite_rc) {
    statement_t* s = malloc_container(statement_t);
    if (s) {
      s->stmt = stmt;
      s->database = d;
      s->prev = NULL;
      s->next = d->statement;
      if (d->statement)
        d->statement->prev = s;
      d->statement = s;
    } else {
      sqlite3_finalize(stmt);
      d->sqlite_rc = SQLITE_NOMEM;
    }
  }
}

static void prepare_cb(uv_async_t* handle) {
  database_t* d = container_of(handle, database_t, async);
  ptr callback = d->callback;
  d->busy = 0;
  d->callback = Svoid;
  free(d->sql);
  d->sql = NULL;
  d->sql_len = 0;
  Sunlock_object(callback);
  int rc = d->sqlite_rc;
  if (SQLITE_OK == rc) {
    osi_add_callback1(callback, Sunsigned((uptr)d->statement));
  } else
    osi_add_callback1(callback, make_sqlite_error("sqlite3_prepare_v2", rc, d->db));
}

ptr osi_prepare_statement(uptr database, ptr sql, ptr callback) {
  database_t* d = (database_t*)database;
  if (d->busy)
    return osi_make_error_pair("osi_prepare_statement", UV_EBUSY);
  if (!Sstringp(sql))
    return osi_make_error_pair("osi_prepare_statement", UV_EINVAL);
  size_t len;
  char* utf8 = osi_string_to_utf8(sql, &len);
  if (!utf8)
    return osi_make_error_pair("osi_prepare_statement", UV_ENOMEM);
  if (len > 0x7fffffff) {
    free(utf8);
    return osi_make_error_pair("sqlite3_prepare_v2", TRANSLATE_SQLITE_ERRNO(SQLITE_TOOBIG));
  }
  d->async.data = prepare_cb;
  Slock_object(callback);
  uv_mutex_lock(&(d->mutex));
  d->busy = 1;
  d->work = prepare_worker;
  d->callback = callback;
  d->sql = utf8;
  d->sql_len = (int)len;
  uv_mutex_unlock(&(d->mutex));
  uv_cond_signal(&(d->cond));
  return Strue;
}

ptr osi_finalize_statement(uptr statement) {
  statement_t* s = (statement_t*)statement;
  if (!s->database) {
    // already closed by close_worker
    free(s);
    return Strue;
  }
  if (s->database->busy)
    return osi_make_error_pair("osi_finalize_statement", UV_EBUSY);
  sqlite3_finalize(s->stmt);
  if (s->prev)
    s->prev->next = s->next;
  else
    s->database->statement = s->next;
  if (s->next)
    s->next->prev = s->prev;
  free(s);
  return Strue;
}

int scheme_to_sql_type(ptr datum) {
  if (Sfalse == datum)
    return SQLITE_NULL;
  if (Sfixnump(datum) || Sbignump(datum))
    return SQLITE_INTEGER;
  if (Sflonump(datum))
    return SQLITE_FLOAT;
  if (Sstringp(datum))
    return SQLITE_TEXT;
  if (Sbytevectorp(datum))
    return SQLITE_BLOB;
  return -1;
}

int set_binding(binding_t* b, ptr datum) {
  int type = scheme_to_sql_type(datum);
  b->type = type;
  switch (type) {
  case SQLITE_NULL:
    return 0;
  case SQLITE_INTEGER:
    b->i = Sinteger64_value(datum);
    return 0;
  case SQLITE_FLOAT:
    b->d = Sflonum_value(datum);
    return 0;
  case SQLITE_TEXT:
    b->utf8 = osi_string_to_utf8(datum, &b->size);
    return b->utf8 ? 0 : UV_ENOMEM;
  case SQLITE_BLOB:
    b->size = Sbytevector_length(datum);
    void* blob = malloc_array(uint8_t, b->size);
    if (!blob)
      return UV_ENOMEM;
    memcpy(blob, Sbytevector_data(datum), b->size);
    b->blob = blob;
    return 0;
  default:
    return UV_EINVAL;
  }
}

ptr get_binding(binding_t* b) {
  switch (b->type) {
  case SQLITE_NULL:
    return Sfalse;
  case SQLITE_INTEGER:
    return Sinteger64(b->i);
  case SQLITE_FLOAT:
    return Sflonum(b->d);
  case SQLITE_TEXT:
    return Sstring_utf8(b->utf8, b->size);
  case SQLITE_BLOB: {
    ptr bv = Smake_bytevector(b->size, 0);
    memcpy(Sbytevector_data(bv), b->blob, b->size);
    return bv;
  }
  default:
    return osi_make_error_pair("osi_get_binding", UV_EINVAL);
  }
}

ptr osi_get_bindings(uptr mbindings) {
  bindings_t* b = (bindings_t*)mbindings;
  int len = (!b) ? 0 : b->len;
  ptr v = Smake_vector(len, Sfixnum(0));
  for (int i = 0; i < len; ++i) {
    ptr x = get_binding(&b->bindings[i]);
    if (Spairp(x))
      return x;
    Svector_set(v, i, x);
  }
  return v;
}

void free_binding(binding_t* b) {
  switch (b->type) {
  case SQLITE_TEXT:
    if (b->utf8) {
      free(b->utf8);
      b->utf8 = NULL;
    }
    return;
  case SQLITE_BLOB:
    if (b->blob) {
      free(b->blob);
      b->blob = NULL;
    }
    return;
  default:
    return;
  }
}

ptr osi_marshal_bindings(ptr bindings) {
  if (Snullp(bindings) || Spairp(bindings)) {
    int len = 0;
    for (ptr p = bindings; p != Snil; p=Scdr(p))
      ++len;
    if (!len)
      return Sunsigned((uptr)NULL);

    bindings_t* mbindings = (bindings_t*)malloc(sizeof(bindings_t)+len*sizeof(binding_t));
    if (!mbindings)
      return osi_make_error_pair("osi_marshal_bindings", UV_ENOMEM);

    mbindings->len = len;
    int i = 0;
    for (ptr p=bindings; p != Snil; p=Scdr(p), ++i) {
      binding_t* b = &mbindings->bindings[i];
      ptr datum = Scar(p);
      int rc = set_binding(b, datum);
      if (rc) {
        for (int j=0; j < i; ++j) {
          free_binding(&mbindings->bindings[j]);
        }
        free(mbindings);
        return osi_make_error_pair("osi_marshal_bindings", rc);
      }
    }
    return Sunsigned((uptr)mbindings);
  } else if (Svectorp(bindings)) {
    int len = (int)Svector_length(bindings);
    if (!len)
      return Sunsigned((uptr)NULL);
    bindings_t* mbindings = (bindings_t*)malloc(sizeof(bindings_t)+len*sizeof(binding_t));
    if (!mbindings)
      return osi_make_error_pair("osi_marshal_bindings", UV_ENOMEM);

    mbindings->len = len;
    for (int i=0; i < len; ++i) {
      binding_t* b = &mbindings->bindings[i];
      ptr datum = Svector_ref(bindings, i);
      int rc = set_binding(b, datum);
      if (rc) {
        for (int j=0; j < i; ++j) {
          free_binding(&mbindings->bindings[j]);
        }
        free(mbindings);
        return osi_make_error_pair("osi_marshal_bindings", rc);
      }
    }
    return Sunsigned((uptr)mbindings);
  }
  return osi_make_error_pair("osi_marshal_bindings", UV_EINVAL);
}

ptr osi_unmarshal_bindings(uptr mbindings) {
  bindings_t* b = (bindings_t*)mbindings;
  if (!b)
    return Strue;
  for (int i=0; i < b->len; ++i) {
    free_binding(&b->bindings[i]);
  }
  free(b);
  return Strue;
}

ptr osi_bind_statement(uptr statement, int index, ptr datum) {
  statement_t* s = (statement_t*)statement;
  if (!s->database)
    return osi_make_error_pair("osi_bind_statement", UV_EINVAL);
  if (s->database->busy)
    return osi_make_error_pair("osi_bind_statement", UV_EBUSY);
  // For strings and blobs, the data must live beyond the scope of
  // this call, so we ask SQLite to copy them. This is in contrast to
  // the bindings in bind_bindings.
  int rc;
  const char* who;
  if (Sfalse == datum) {
    who = "sqlite3_bind_null";
    rc = sqlite3_bind_null(s->stmt, index);
  } else if (Sfixnump(datum) || Sbignump(datum)) {
    who = "sqlite3_bind_int64";
    rc = sqlite3_bind_int64(s->stmt, index, Sinteger64_value(datum));
  } else if (Sflonump(datum)) {
    who = "sqlite3_bind_double";
    rc = sqlite3_bind_double(s->stmt, index, Sflonum_value(datum));
  } else if (Sstringp(datum)) {
    size_t len;
    char* utf8 = osi_string_to_utf8(datum, &len);
    if (!utf8)
      return osi_make_error_pair("osi_bind_statement", UV_ENOMEM);
    who = "sqlite3_bind_text64";
    rc = sqlite3_bind_text64(s->stmt, index, utf8, len, free, SQLITE_UTF8);
  } else if (Sbytevectorp(datum)) {
    who = "sqlite3_bind_blob64";
    rc = sqlite3_bind_blob64(s->stmt, index, (const void*)Sbytevector_data(datum), Sbytevector_length(datum), SQLITE_TRANSIENT);
  } else
    return osi_make_error_pair("osi_bind_statement", UV_EINVAL);
  if (SQLITE_OK != rc)
    return make_sqlite_error(who, rc, s->database->db);
  return Strue;
}

static int bind_bindings(statement_t* s, bindings_t* b, const char** who) {
  // For strings and blobs, the memory is managed by the lifetime of
  // the binding_t records, so we do not ask SQLite to copy them. This
  // is in contrast to the bindings in osi_bind_statement.
  int rc = SQLITE_OK;
  *who = NULL;
  if (!b)
    return rc;
  for (int j=0; j < b->len; ++j) {
    binding_t* curr = &b->bindings[j];
    switch (curr->type) {
    case SQLITE_NULL:
      *who = "sqlite3_bind_null";
      rc = sqlite3_bind_null(s->stmt, j+1);
      break;
    case SQLITE_INTEGER:
      *who = "sqlite3_bind_int64";
      rc = sqlite3_bind_int64(s->stmt, j+1, curr->i);
      break;
    case SQLITE_FLOAT:
      *who = "sqlite3_bind_double";
      rc = sqlite3_bind_double(s->stmt, j+1, curr->d);
      break;
    case SQLITE_TEXT:
      *who = "sqlite3_bind_text64";
      rc = sqlite3_bind_text64(s->stmt, j+1, curr->utf8, curr->size, SQLITE_STATIC, SQLITE_UTF8);
      break;
    case SQLITE_BLOB:
      *who = "sqlite3_bind_blob";
      rc = sqlite3_bind_blob64(s->stmt, j+1, curr->blob, curr->size, SQLITE_STATIC);
      break;
    }
  }
  return rc;
}

ptr osi_bind_statement_bindings(uptr statement, uptr mbindings) {
  statement_t* s = (statement_t*)statement;
  bindings_t* b = (bindings_t*)mbindings;
  if (!s->database)
    return osi_make_error_pair("osi_bind_statement_bindings", UV_EINVAL);
  if (s->database->busy)
    return osi_make_error_pair("osi_bind_statement_bindings", UV_EBUSY);
  const char* who = NULL;
  int rc = bind_bindings(s, b, &who);
  if (SQLITE_OK != rc)
    return make_sqlite_error(who, rc, s->database->db);
  return Strue;
}

ptr osi_clear_statement_bindings(uptr statement) {
  statement_t* s = (statement_t*)statement;
  if (!s->database)
    return osi_make_error_pair("osi_clear_statement_bindings", UV_EINVAL);
  if (s->database->busy)
    return osi_make_error_pair("osi_clear_statement_bindings", UV_EBUSY);
  int rc = sqlite3_clear_bindings(s->stmt);
  if (SQLITE_OK != rc)
    return make_sqlite_error("osi_clear_statement_bindings", rc, s->database->db);
  return Strue;
}

ptr osi_get_last_insert_rowid(uptr database) {
  database_t* d = (database_t*)database;
  if (d->busy)
    return osi_make_error_pair("osi_get_last_insert_rowid", UV_EBUSY);
  return Sinteger64(sqlite3_last_insert_rowid(d->db));
}

ptr osi_get_statement_columns(uptr statement) {
  statement_t* s = (statement_t*)statement;
  if (!s->database)
    return osi_make_error_pair("osi_get_statement_columns", UV_EINVAL);
  if (s->database->busy)
    return osi_make_error_pair("osi_get_statement_columns", UV_EBUSY);
  int count = sqlite3_column_count(s->stmt);
  ptr v = Smake_vector(count, Sfixnum(0));
  for (int i=0; i < count; ++i)
    Svector_set(v, i, Sstring_utf8(sqlite3_column_name(s->stmt, i), -1));
  return v;
}

ptr osi_get_statement_expanded_sql(uptr statement) {
  statement_t* s = (statement_t*)statement;
  if (!s->database)
    return osi_make_error_pair("osi_get_statement_expanded_sql", UV_EINVAL);
  if (s->database->busy)
    return osi_make_error_pair("osi_get_statement_expanded_sql", UV_EBUSY);
  char* sql = sqlite3_expanded_sql(s->stmt);
  if (!sql)
    return osi_make_error_pair("osi_get_statement_expanded_sql", UV_ENOMEM);
  ptr r = Sstring_utf8(sql, -1);
  sqlite3_free(sql);
  return r;
}

ptr osi_reset_statement(uptr statement) {
  statement_t* s = (statement_t*)statement;
  if (!s->database)
    return osi_make_error_pair("osi_reset_statement", UV_EINVAL);
  if (s->database->busy)
    return osi_make_error_pair("osi_reset_statement", UV_EBUSY);
  int rc = sqlite3_reset(s->stmt);
  if (SQLITE_OK != rc)
    return make_sqlite_error("sqlite3_reset", rc, s->database->db);
  return Strue;
}

static void step_worker(void* arg) {
  database_t* d = (database_t*)arg;
  d->sqlite_rc = sqlite3_step(d->stmt);
}

static void step_cb(uv_async_t* handle) {
  database_t* d = container_of(handle, database_t, async);
  ptr callback = d->callback;
  d->busy = 0;
  d->callback = Svoid;
  sqlite3_stmt* stmt = d->stmt;
  d->stmt = NULL;
  Sunlock_object(callback);
  int rc = d->sqlite_rc;
  ptr arg;
  if (SQLITE_DONE == rc) {
    arg = Sfalse;
  } else if (SQLITE_ROW == rc) {
    int n = sqlite3_column_count(stmt);
    arg = Smake_vector(n, Sfixnum(0));
    for (int i = 0; i < n; i++) {
      ptr x;
      switch (sqlite3_column_type(stmt, i)) {
      case SQLITE_NULL:
        x = Sfalse;
        break;
      case SQLITE_INTEGER:
        x = Sinteger64(sqlite3_column_int64(stmt, i));
        break;
      case SQLITE_FLOAT:
        x = Sflonum(sqlite3_column_double(stmt, i));
        break;
      case SQLITE_TEXT: {
        x = Sstring_utf8((const char*)sqlite3_column_text(stmt, i),
                         sqlite3_column_bytes(stmt, i));
        break;
      }
      default: { // SQLITE_BLOB
        const void* blob = sqlite3_column_blob(stmt, i);
        int n = sqlite3_column_bytes(stmt, i);
        x = Smake_bytevector(n, 0);
        memcpy(Sbytevector_data(x), blob, n);
      }
      }
      Svector_set(arg, i, x);
    }
  } else {
    arg = make_sqlite_error("sqlite3_step", rc, d->db);
  }
  osi_add_callback1(callback, arg);
}

ptr osi_step_statement(uptr statement, ptr callback) {
  statement_t* s = (statement_t*)statement;
  database_t* d = s->database;
  if (!d)
    return osi_make_error_pair("osi_step_statement", UV_EINVAL);
  if (d->busy)
    return osi_make_error_pair("osi_step_statement", UV_EBUSY);
  d->async.data = step_cb;
  Slock_object(callback);
  uv_mutex_lock(&(d->mutex));
  d->busy = 1;
  d->work = step_worker;
  d->callback = callback;
  d->stmt = s->stmt;
  uv_mutex_unlock(&(d->mutex));
  uv_cond_signal(&(d->cond));
  return Strue;
}

ptr osi_interrupt_database(uptr database) {
  sqlite3_interrupt(((database_t*)database)->db);
  return ((database_t*)database)->busy ? Strue : Sfalse;
}

ptr osi_get_sqlite_status(int operation, int resetp) {
  sqlite3_int64 current;
  sqlite3_int64 highwater;
  int sqlite_rc = sqlite3_status64(operation, &current, &highwater, resetp);
  if (SQLITE_OK != sqlite_rc)
    return osi_make_error_pair("sqlite3_status", TRANSLATE_SQLITE_ERRNO(sqlite_rc));
  ptr v = Smake_vector(2, Sfixnum(0));
  Svector_set(v, 0, Sinteger64(current));
  Svector_set(v, 1, Sinteger64(highwater));
  return v;
}

static void bulk_worker(void* arg) {
  database_t* d = (database_t*)arg;
  bulk_t* bulk = d->bulk;
  statement_t** statements = bulk->statements;
  bindings_t** bindings = bulk->mbindings;

  d->sqlite_rc = SQLITE_OK;
  for (int i=0; i < bulk->count; ++i) {
    statement_t* s = statements[i];
    sqlite3_reset(s->stmt);
    int rc = bind_bindings(s, bindings[i], &bulk->who);
    if (SQLITE_OK != rc) {
      d->sqlite_rc = rc;
      return;
    }

    rc = sqlite3_step(s->stmt);
    switch (rc) {
    case SQLITE_OK:
    case SQLITE_DONE:
    case SQLITE_ROW:
      break;
    default:
      d->sqlite_rc = rc;
      return;
    }

    sqlite3_clear_bindings(s->stmt);
    sqlite3_reset(s->stmt);
  }
}

static void bulk_cb(uv_async_t* handle) {
  database_t* d = container_of(handle, database_t, async);
  ptr callback = d->callback;
  d->busy = 0;
  d->callback = Svoid;
  Sunlock_object(callback);
  bulk_t* bulk = d->bulk;
  d->bulk = NULL;
  const char* who = bulk->who;
  free(bulk->mbindings);
  free(bulk->statements);
  free(bulk);

  int rc = d->sqlite_rc;
  ptr arg;
  switch (rc) {
  case SQLITE_OK:
  case SQLITE_DONE:
  case SQLITE_ROW:
    arg = Strue;
    break;
  default:
    arg = make_sqlite_error(who ? who : "osi_bulk_execute", rc, d->db);
    break;
  }
  osi_add_callback1(callback, arg);
}

ptr osi_bulk_execute(ptr statements, ptr mbindings, ptr callback) {
  if (!Svectorp(statements) ||
      !Svectorp(mbindings) ||
      (Svector_length(statements) != Svector_length(mbindings)))
    return osi_make_error_pair("osi_bulk_execute", UV_EINVAL);
  if (Svector_length(statements) < 1)
    return osi_make_error_pair("osi_bulk_execute", UV_EINVAL);

  uptr statement = (uptr)Sunsigned_value(Svector_ref(statements, 0));
  statement_t* s = (statement_t*)statement;
  database_t* d = s->database;
  if (!d)
    return osi_make_error_pair("osi_bulk_execute", UV_EINVAL);
  if (d->busy)
    return osi_make_error_pair("osi_bulk_execute", UV_EBUSY);

  bulk_t* bulk = malloc_container(bulk_t);
  if (!bulk)
    return osi_make_error_pair("osi_bulk_execute", UV_ENOMEM);
  int count = (int)Svector_length(statements);
  bulk->count = count;
  bulk->statements = malloc_array(statement_t*, count);
  if (!bulk->statements) {
    free(bulk);
    return osi_make_error_pair("osi_bulk_execute", UV_ENOMEM);
  }
  bulk->mbindings = malloc_array(bindings_t*, count);
  if (!bulk->mbindings) {
    free(bulk->statements);
    free(bulk);
    return osi_make_error_pair("osi_bulk_execute", UV_ENOMEM);
  }
  bulk->who = NULL;

  for (int i=0; i < count; ++i) {
    bulk->statements[i] = (statement_t*)Sunsigned_value(Svector_ref(statements, i));
    bulk->mbindings[i] = (bindings_t*)Sunsigned_value(Svector_ref(mbindings, i));
  }
  Slock_object(callback);
  uv_mutex_lock(&(d->mutex));
  d->async.data = bulk_cb;
  d->busy = 1;
  d->work = bulk_worker;
  d->callback = callback;
  d->bulk = bulk;
  uv_mutex_unlock(&(d->mutex));
  uv_cond_signal(&(d->cond));
  return Strue;
}
