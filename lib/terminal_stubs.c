/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <errno.h>

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifndef _WIN32
#include <signal.h>
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

/* Signals that k7 ignores, such as SIGPIPE under Eio, or blocks would stay
   so across exec. */
CAMLprim value native_default_signals(value unit) {
  CAMLparam1(unit);
#ifndef _WIN32
  sigset_t none;
  for (int s = 1; s < NSIG; s++)
    if (s != SIGKILL && s != SIGSTOP) signal(s, SIG_DFL);
  sigemptyset(&none);
  sigprocmask(SIG_SETMASK, &none, NULL);
#endif
  CAMLreturn(Val_unit);
}
