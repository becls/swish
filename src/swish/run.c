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
#include <errno.h>
#include <string.h>
#include <stdlib.h>

static void (* g_aux_init)(void) = NULL;

static void swish_init(void) {
#define add_foreign(sym) Sforeign_symbol(#sym, (void *)sym)
  add_foreign(osi_bind_statement);
  add_foreign(osi_chmod);
  add_foreign(osi_clear_statement_bindings);
  add_foreign(osi_close_SHA1);
  add_foreign(osi_close_database);
  add_foreign(osi_close_path_watcher);
  add_foreign(osi_close_port);
  add_foreign(osi_close_tcp_listener);
  add_foreign(osi_connect_tcp);
  add_foreign(osi_exit);
  add_foreign(osi_finalize_statement);
  add_foreign(osi_get_SHA1);
  add_foreign(osi_get_argv);
  add_foreign(osi_get_bytes_used);
  add_foreign(osi_get_callbacks);
  add_foreign(osi_get_error_text);
  add_foreign(osi_get_executable_path);
  add_foreign(osi_get_file_size);
  add_foreign(osi_get_hostname);
  add_foreign(osi_get_hrtime);
  add_foreign(osi_get_ip_address);
  add_foreign(osi_get_last_insert_rowid);
  add_foreign(osi_get_pid);
  add_foreign(osi_get_real_path);
  add_foreign(osi_get_sqlite_status);
  add_foreign(osi_get_stat);
  add_foreign(osi_get_statement_columns);
  add_foreign(osi_get_statement_expanded_sql);
  add_foreign(osi_get_tcp_listener_port);
  add_foreign(osi_get_temp_directory);
  add_foreign(osi_get_time);
  add_foreign(osi_hash_data);
  add_foreign(osi_init);
  add_foreign(osi_interrupt_database);
  add_foreign(osi_is_quantum_over);
  add_foreign(osi_kill);
  add_foreign(osi_list_directory);
  add_foreign(osi_list_uv_handles);
  add_foreign(osi_listen_tcp);
  add_foreign(osi_make_directory);
  add_foreign(osi_make_uuid);
  add_foreign(osi_open_SHA1);
  add_foreign(osi_open_database);
  add_foreign(osi_open_fd);
  add_foreign(osi_open_file);
  add_foreign(osi_prepare_statement);
  add_foreign(osi_read_port);
  add_foreign(osi_remove_directory);
  add_foreign(osi_rename);
  add_foreign(osi_reset_statement);
  add_foreign(osi_set_quantum);
  add_foreign(osi_start_signal);
  add_foreign(osi_stop_signal);
  add_foreign(osi_spawn);
  add_foreign(osi_step_statement);
  add_foreign(osi_unlink);
  add_foreign(osi_watch_path);
  add_foreign(osi_write_port);
  if (g_aux_init) g_aux_init();
}

// We expect to find our boot file alongside the executable.
static char* get_boot_fn() {
  char execfn[32768];
  size_t n = sizeof(execfn);
  int rc = uv_exepath(execfn, &n);
  if (rc < 0) {
    perror("uv_exepath");
    exit(1);
  }

  const char* suffix = ".boot";
  size_t suffixlen = strlen(suffix);
  size_t pathlast = strlen(execfn);
  char* bootfn = (char*)malloc(sizeof(char) * (pathlast + suffixlen + 1));
  if (NULL == bootfn) {
    fprintf(stderr, "malloc failed\n");
    exit(1);
  }
#ifdef _WIN32
  const char* end = strrchr(execfn, '.');
  const char* exe = ".exe";
  if (NULL != end && _strnicmp(end, exe, 5) == 0) {
    pathlast -=  strlen(exe);
  }
#endif
  memcpy(bootfn, execfn, pathlast);
  memcpy((char*)(bootfn + pathlast), suffix, suffixlen);
  bootfn[pathlast+suffixlen] = '\0';
  return bootfn;
}

static int allow_verbose_flag(const char* bootfn) {
  size_t len = strlen(bootfn);
#if _WIN32
  const char* suffix =  "\\swish.boot";
#define COMPARE _strnicmp
#else
  const char* suffix =  "/swish.boot";
#define COMPARE strncmp
#endif
  size_t suffixlen = strlen(suffix);
  return len >= suffixlen && !COMPARE(bootfn + len - suffixlen, suffix, suffixlen);
}

// custom_init may be NULL or a pointer to a function that performs application-specific
// initialization during Sbuild_heap.
int swish_run(int argc, const char* argv[], void (*custom_init)(void)) {
  char* bootfn = get_boot_fn();

  Sscheme_init(NULL);
  // Don't interfere with swish scripts or stand-alone swish applications that
  // want to support a --verbose option.
  if (argc >= 2 && (strcmp(argv[1], "--verbose") == 0) && allow_verbose_flag(bootfn)) {
    Sset_verbose(1);
  }
  Sregister_boot_file(bootfn);

  osi_set_argv(argc, argv);
  g_aux_init = custom_init;
  Sbuild_heap(argv[0], swish_init);
  free(bootfn);

  int status = Sscheme_start(argc, argv);
  Sscheme_deinit();
  exit(status);
}
