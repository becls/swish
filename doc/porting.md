# Porting from Swish for Windows

## Libraries

* `(swish app-io)`
  * removed `bin-dir`
  * renamed `log-path` to `log-file`
  * renamed `tmp-path` to `tmp-dir`
  * renamed `web-path` to `web-dir`
* `(swish erlang)`
  * added `complete-io` and `wait-for-io`
  * `exit` is now `raise` to eliminate a conflict with Chez Scheme.
  * `define-record` is now `define-tuple` to eliminate a conflict with
    Chez Scheme.
  * `erlang:now` no longer drifts from clock time.
* `(swish gen-server)`
  * `define-state-record` is now `define-state-tuple`
* `(swish io)`
  * removed constants `CREATE_ALWAYS`, `CREATE_NEW`,
    `FILE_SHARE_DELETE`, `FILE_SHARE_NONE`, `FILE_SHARE_READ`,
    `FILE_SHARE_WRITE`, `GENERIC_READ`, `GENERIC_WRITE`,
    `OPEN_ALWAYS`, `OPEN_EXISTING`, `TRUNCATE_EXISTING`
  * removed `absolute-path`
  * removed `accept-tcp`, see `listen-tcp`
  * removed `close-directory-watcher`, see `close-path-watcher`
  * removed `connect-usb`
  * removed `create-client-pipe`
  * removed `create-directory-path`, see `make-directory-path`
  * removed `create-file`, see `open-file`
  * removed `create-file-port`, see `open-file-port`
  * removed `create-server-pipe`
  * removed `create-watched-process`, see `spawn-os-process`
  * removed `directory-watcher-path`, see `path-watcher-path`
  * removed `find-files`, see `list-directory`
  * removed `move-file`, see `rename-path`
  * removed `watch-directory`, see `watch-path`
  * added `<stat>` tuple
  * added directory-entry constants `DIRENT_BLOCK`, `DIRENT_CHAR`,
    `DIRENT_DIR`, `DIRENT_FIFO`, `DIRENT_FILE`, `DIRENT_LINK`,
    `DIRENT_SOCKET`, `DIRENT_UNKNOWN`
  * added file open constants `O_APPEND`, `O_CREAT`, `O_DIRECT`,
    `O_DIRECTORY`, `O_DSYNC`, `O_EXCL`, `O_EXLOCK`, `O_NOATIME`,
    `O_NOCTTY`, `O_NOFOLLOW`, `O_NONBLOCK`, `O_RANDOM`, `O_RDONLY`,
    `O_RDWR`, `O_SEQUENTIAL`, `O_SHORT_LIVED`, `O_SYMLINK`, `O_SYNC`,
    `O_TEMPORARY`, `O_TRUNC`, `O_WRONLY`
  * added stat mode constants `S_IFBLK`, `S_IFCHR`, `S_IFDIR`,
    `S_IFIFO`, `S_IFLNK`, `S_IFMT`, `S_IFREG`, `S_IFSOCK`
  * added constant `UV_EOF`
  * added `close-path-watcher`
  * added `directory?`
  * added `get-stat`
  * added `list-directory`
  * `listen-tcp` now accepts automatically and takes an address.
    Use "::" for all IPv4 and IPv6 addresses.
  * added `listener?`
  * added `make-directory`
  * added `make-directory-path`
  * added `open-file`
  * added `open-file-port`
  * added `path-watcher-path`
  * added `path-watcher?`
  * added `regular-file?`
  * added `remove-directory`
  * added `remove-file`
  * added `rename-path`
  * added `set-file-mode`
  * added `spawn-os-process`
  * added `stat-directory?`
  * added `stat-regular-file?`
  * added `watch-path`
* `(swish meta)`
  * removed redundant `make-id`; use `compound-id`
* `(swish osi)` was completely revamped.
