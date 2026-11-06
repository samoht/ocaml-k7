/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#ifndef _WIN32
#include <sys/ioctl.h>
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Windows has no TIOCGWINSZ, and k7 records nothing there. */
CAMLprim value k7_window_size(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(size);
#ifdef _WIN32
  CAMLreturn(Val_none);
#else
  struct winsize ws;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) != 0 || ws.ws_col == 0 ||
      ws.ws_row == 0)
    CAMLreturn(Val_none);
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(ws.ws_col));
  Store_field(size, 1, Val_int(ws.ws_row));
  CAMLreturn(caml_alloc_some(size));
#endif
}
