#if !defined(_OCAMLFD_H)
#define _OCAMLFD_H

#if defined(__MINGW32__)
#include <w32api/winsock.h>

struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  enum { KIND_HANDLE, KIND_SOCKET } kind;
};

#define Fd_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v) (((struct filedescr *) Data_custom_val(v))->fd.socket)

typedef HANDLE OS_FD;
typedef unsigned int uint;
extern void win32_maperr(unsigned long errcode);

#else


#define Fd_val(v) Int_val(v)
#define Socket_val(v) Int_val(v)

typedef int OS_FD;

#endif

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/memory.h"


#include <errno.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <signal.h>

#ifdef HAS_SIGNALS_H
#include <signals.h>
#endif

#include <sys/types.h>
#include <sys/time.h>

#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif

#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAS_UNISTD
#include <unistd.h>
#else
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#if defined(__OpenBSD__) || defined(__FreeBSD__)
#include <string.h>
#endif

#if !defined(__MINGW32__)
#include <sys/mman.h>
#endif


#define Nothing ((value) 0)

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

static int seek_command_table[] = {
  SEEK_SET, SEEK_CUR, SEEK_END
};


extern int os_lseek(OS_FD fd, int pos, int dir);
extern int os_read(OS_FD fd, char *buf, int len);
extern void os_ftruncate(OS_FD fd, int len);
extern int os_getdtablesize();
extern int os_getfdsize(OS_FD fd);
extern int os_getfilesize(char *path);




#endif
