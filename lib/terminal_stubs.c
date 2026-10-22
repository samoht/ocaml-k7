/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <errno.h>

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifndef _WIN32
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#endif

/* Temporary, until matrix's Pty.spawn keeps the slave open: delete it with
   [slave] in native.ml. */
CAMLprim value native_acquire_terminal(value fd) {
  CAMLparam1(fd);
#ifdef _WIN32
  caml_unix_error(ENOSYS, "acquire_terminal", Nothing);
#else
  if (setsid() == -1) caml_uerror("setsid", Nothing);
  if (ioctl(Int_val(fd), TIOCSCTTY, 0) == -1)
    caml_uerror("ioctl(TIOCSCTTY)", Nothing);
#endif
  CAMLreturn(Val_unit);
}
