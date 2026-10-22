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

/* Temporary: matrix 0.1.0's Pty.spawn closes the slave side of the
   terminal in the parent, and macOS then loses the output of a program
   that prints and exits at once. Until matrix fixes that (upstream issue
   to file: "matrix.pty: Pty.spawn closes the slave, so macOS loses the
   output of a program that exits at once"), k7 forks the program itself
   and keeps the slave, and needs this stub for what matrix does not
   expose. Delete this file, its foreign_stubs stanza in lib/dune and
   acquire_terminal in lib/native.ml once k7 can use Pty.spawn again. */

/* Start a new session whose controlling terminal is [fd]. */
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
