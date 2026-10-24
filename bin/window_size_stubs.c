/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <sys/ioctl.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value k7_window_size(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(size);
  struct winsize ws;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) != 0 || ws.ws_col == 0 ||
      ws.ws_row == 0)
    CAMLreturn(Val_none);
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(ws.ws_col));
  Store_field(size, 1, Val_int(ws.ws_row));
  CAMLreturn(caml_alloc_some(size));
}
