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

#include <stdio.h>
#include <fcntl.h>
#include "uv.h"
#define XX(name,flag) printf("(define-export "name" %d)\n", flag)
#define XUV(id) XX(#id,UV_##id)
#define XFS(id) XX(#id,UV_FS_##id)
#ifdef _WIN32
#define XFM(id) XX(#id,_##id)
#define _S_IFBLK 0
#define _S_IFLNK S_IFLNK
#define _S_IFSOCK 0
#else
#define XFM(id) XX(#id,id)
#endif

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;

  XUV(DIRENT_UNKNOWN);
  XUV(DIRENT_FILE);
  XUV(DIRENT_DIR);
  XUV(DIRENT_LINK);
  XUV(DIRENT_FIFO);
  XUV(DIRENT_SOCKET);
  XUV(DIRENT_CHAR);
  XUV(DIRENT_BLOCK);

  XFS(O_APPEND);
  XFS(O_CREAT);
  XFS(O_DIRECT);
  XFS(O_DIRECTORY);
  XFS(O_DSYNC);
  XFS(O_EXCL);
  XFS(O_EXLOCK);
  XFS(O_NOATIME);
  XFS(O_NOCTTY);
  XFS(O_NOFOLLOW);
  XFS(O_NONBLOCK);
  XFS(O_RANDOM);
  XFS(O_RDONLY);
  XFS(O_RDWR);
  XFS(O_SEQUENTIAL);
  XFS(O_SHORT_LIVED);
  XFS(O_SYMLINK);
  XFS(O_SYNC);
  XFS(O_TEMPORARY);
  XFS(O_TRUNC);
  XFS(O_WRONLY);

  XFM(S_IFMT);
  XFM(S_IFIFO);
  XFM(S_IFCHR);
  XFM(S_IFDIR);
  XFM(S_IFBLK);
  XFM(S_IFREG);
  XFM(S_IFLNK);
  XFM(S_IFSOCK);

  printf("(define-export UV_EOF %d)\n", UV_EOF);

  return 0;
}
