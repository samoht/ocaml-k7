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
#if defined(__APPLE__)
#include <libproc.h>
#elif defined(__linux__)
#include <sys/syscall.h>
#endif
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

#ifndef _WIN32
static void close_range_from(int low, int high) {
  for (int fd = low; fd < high; fd++) close(fd);
}
#endif

/* Closes every descriptor above 2 but [keep]. */
CAMLprim value native_close_from_3(value keep) {
  CAMLparam1(keep);
#ifndef _WIN32
  int k = Int_val(keep);
#if defined(__APPLE__)
  struct proc_fdinfo fds[4096];
  int n = proc_pidinfo(getpid(), PROC_PIDLISTFDS, 0, fds, sizeof fds);
  if (n > 0 && n < (int)sizeof fds) {
    for (int i = 0; i < n / (int)PROC_PIDLISTFD_SIZE; i++)
      if (fds[i].proc_fd > 2 && fds[i].proc_fd != k) close(fds[i].proc_fd);
    CAMLreturn(Val_unit);
  }
#elif defined(__linux__) && defined(SYS_close_range)
  if (k <= 2) {
    if (syscall(SYS_close_range, 3, ~0U, 0) == 0) CAMLreturn(Val_unit);
  } else if (syscall(SYS_close_range, 3, k - 1, 0) == 0 &&
             syscall(SYS_close_range, k + 1, ~0U, 0) == 0)
    CAMLreturn(Val_unit);
#endif
  long max = sysconf(_SC_OPEN_MAX);
  if (max < 0 || max > 1 << 20) max = 1 << 20;
  if (k > 2) {
    close_range_from(3, k);
    close_range_from(k + 1, (int)max);
  } else
    close_range_from(3, (int)max);
#endif
  CAMLreturn(Val_unit);
}
