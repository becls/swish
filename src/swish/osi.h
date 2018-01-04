// Copyright 2017 Beckman Coulter, Inc.
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

#ifdef _WIN32
#define WINVER 0x0601 // Windows 7
#define _WIN32_WINNT WINVER
#include "uv.h"
#define SCHEME_IMPORT
#include "scheme.h"
#define strdup _strdup
#undef EXPORT
#define EXPORT extern __declspec (dllexport)
#else
#include "uv.h"
#include "scheme.h"
#include <stdlib.h>
#include <unistd.h>
#include <uuid/uuid.h>
#endif
#ifdef __APPLE__
#include <malloc/malloc.h>
#else
#include <malloc.h>
#endif
#include <string.h>
#include "sqlite3.h"

uv_loop_t* g_loop;

void add_callback1(ptr callback, ptr arg);
void add_callback2(ptr callback, ptr arg1, ptr arg2);
ptr make_error_pair(const char* who, int error);
ptr make_scheme_string(const char* utf8);

// Event Loop
EXPORT ptr osi_get_callbacks(uint64_t timeout);
EXPORT void osi_set_tick(uint64_t nanoseconds);
EXPORT int osi_is_tick_over(void);

// Ports
EXPORT ptr osi_read_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback);
EXPORT ptr osi_write_port(uptr port, ptr buffer, size_t start_index, uint32_t size, int64_t offset, ptr callback);
EXPORT ptr osi_close_port(uptr port, ptr callback);

// Process
EXPORT void osi_exit(int status);

// File System
EXPORT ptr osi_open_file(const char* path, int flags, int mode, ptr callback);
EXPORT ptr osi_get_file_size(uptr port, ptr callback);
EXPORT uptr osi_stdin(void);
EXPORT ptr osi_chmod(const char* path, int mode, ptr callback);
EXPORT ptr osi_make_directory(const char* path, int mode, ptr callback);
EXPORT ptr osi_list_directory(const char* path, ptr callback);
EXPORT ptr osi_remove_directory(const char* path, ptr callback);
EXPORT ptr osi_rename(const char* path, const char* new_path, ptr callback);
EXPORT ptr osi_stat(const char* path, int follow, ptr callback);
EXPORT ptr osi_unlink(const char* path, ptr callback);
EXPORT ptr osi_watch_path(const char* path, ptr callback);
EXPORT void osi_close_path_watcher(uptr watcher);

// TCP/IP

EXPORT ptr osi_connect_tcp(const char* node, const char* service, ptr callback);
EXPORT ptr osi_listen_tcp(uint16_t port, ptr callback);
EXPORT void osi_close_tcp_listener(uptr listener);
EXPORT ptr osi_get_tcp_listener_port(uptr listener);
EXPORT ptr osi_get_ip_address(uptr port);

// Information
EXPORT size_t osi_bytes_used(void);
EXPORT ptr osi_make_guid(void);
EXPORT ptr osi_hostname(void);
EXPORT uint64_t osi_hrtime(void);
EXPORT uint64_t osi_now(void);
EXPORT const char* osi_strerror(int err);
EXPORT void osi_print_all_handles(void);

// SQLite
EXPORT ptr osi_open_database(const char* filename, int flags, ptr callback);
EXPORT ptr osi_close_database(uptr database, ptr callback);
EXPORT ptr osi_prepare_statement(uptr database, ptr sql, ptr callback);
EXPORT ptr osi_finalize_statement(uptr statement);
EXPORT ptr osi_bind_statement(uptr statement, int index, ptr datum);
EXPORT ptr osi_clear_statement_bindings(uptr statement);
EXPORT ptr osi_get_last_insert_rowid(uptr database);
EXPORT ptr osi_get_statement_columns(uptr statement);
EXPORT ptr osi_get_statement_sql(uptr statement);
EXPORT ptr osi_reset_statement(uptr statement);
EXPORT ptr osi_step_statement(uptr statement, ptr callback);
EXPORT void osi_interrupt_database(uptr database);
EXPORT ptr osi_get_sqlite_status(int operation, int resetp);

#define TRANSLATE_SQLITE_ERRNO(x) (-(6000000 + x))

#define SQLITE_ERRNO_MAP(XX)\
  XX(SQLITE_ERROR)\
  XX(SQLITE_INTERNAL)\
  XX(SQLITE_PERM)\
  XX(SQLITE_ABORT)\
  XX(SQLITE_ABORT_ROLLBACK)\
  XX(SQLITE_BUSY)\
  XX(SQLITE_BUSY_RECOVERY)\
  XX(SQLITE_BUSY_SNAPSHOT)\
  XX(SQLITE_LOCKED)\
  XX(SQLITE_LOCKED_SHAREDCACHE)\
  XX(SQLITE_NOMEM)\
  XX(SQLITE_READONLY)\
  XX(SQLITE_READONLY_RECOVERY)\
  XX(SQLITE_READONLY_CANTLOCK)\
  XX(SQLITE_READONLY_ROLLBACK)\
  XX(SQLITE_READONLY_DBMOVED)\
  XX(SQLITE_INTERRUPT)\
  XX(SQLITE_IOERR)\
  XX(SQLITE_IOERR_READ)\
  XX(SQLITE_IOERR_SHORT_READ)\
  XX(SQLITE_IOERR_WRITE)\
  XX(SQLITE_IOERR_FSYNC)\
  XX(SQLITE_IOERR_DIR_FSYNC)\
  XX(SQLITE_IOERR_TRUNCATE)\
  XX(SQLITE_IOERR_FSTAT)\
  XX(SQLITE_IOERR_UNLOCK)\
  XX(SQLITE_IOERR_RDLOCK)\
  XX(SQLITE_IOERR_DELETE)\
  XX(SQLITE_IOERR_NOMEM)\
  XX(SQLITE_IOERR_ACCESS)\
  XX(SQLITE_IOERR_CHECKRESERVEDLOCK)\
  XX(SQLITE_IOERR_LOCK)\
  XX(SQLITE_IOERR_CLOSE)\
  XX(SQLITE_IOERR_DIR_CLOSE)\
  XX(SQLITE_IOERR_SHMOPEN)\
  XX(SQLITE_IOERR_SHMSIZE)\
  XX(SQLITE_IOERR_SHMLOCK)\
  XX(SQLITE_IOERR_SHMMAP)\
  XX(SQLITE_IOERR_SEEK)\
  XX(SQLITE_IOERR_DELETE_NOENT)\
  XX(SQLITE_IOERR_MMAP)\
  XX(SQLITE_IOERR_GETTEMPPATH)\
  XX(SQLITE_IOERR_CONVPATH)\
  XX(SQLITE_CORRUPT)\
  XX(SQLITE_CORRUPT_VTAB)\
  XX(SQLITE_NOTFOUND)\
  XX(SQLITE_FULL)\
  XX(SQLITE_CANTOPEN)\
  XX(SQLITE_CANTOPEN_NOTEMPDIR)\
  XX(SQLITE_CANTOPEN_ISDIR)\
  XX(SQLITE_CANTOPEN_FULLPATH)\
  XX(SQLITE_CANTOPEN_CONVPATH)\
  XX(SQLITE_PROTOCOL)\
  XX(SQLITE_EMPTY)\
  XX(SQLITE_SCHEMA)\
  XX(SQLITE_TOOBIG)\
  XX(SQLITE_CONSTRAINT)\
  XX(SQLITE_CONSTRAINT_UNIQUE)\
  XX(SQLITE_CONSTRAINT_TRIGGER)\
  XX(SQLITE_CONSTRAINT_FOREIGNKEY)\
  XX(SQLITE_CONSTRAINT_CHECK)\
  XX(SQLITE_CONSTRAINT_PRIMARYKEY)\
  XX(SQLITE_CONSTRAINT_NOTNULL)\
  XX(SQLITE_CONSTRAINT_COMMITHOOK)\
  XX(SQLITE_CONSTRAINT_VTAB)\
  XX(SQLITE_CONSTRAINT_FUNCTION)\
  XX(SQLITE_CONSTRAINT_ROWID)\
  XX(SQLITE_MISMATCH)\
  XX(SQLITE_MISUSE)\
  XX(SQLITE_NOLFS)\
  XX(SQLITE_AUTH)\
  XX(SQLITE_FORMAT)\
  XX(SQLITE_RANGE)\
  XX(SQLITE_NOTADB)\
  XX(SQLITE_ROW)\
  XX(SQLITE_NOTICE)\
  XX(SQLITE_NOTICE_RECOVER_WAL)\
  XX(SQLITE_NOTICE_RECOVER_ROLLBACK)\
  XX(SQLITE_WARNING)\
  XX(SQLITE_WARNING_AUTOINDEX)\
  XX(SQLITE_DONE)

#define container_of(p, t, member) ((t*)((char*)(p)-offsetof(t, member)))
#define malloc_container(t) ((t*)malloc(sizeof(t)))
