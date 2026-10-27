/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <errno.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifndef _WIN32
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#if defined(__APPLE__)
#include <libproc.h>
#elif defined(__linux__)
#include <sys/syscall.h>
#endif
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>
#endif

#ifndef _WIN32
static void close_range_from(int low, int high) {
  for (int fd = low; fd < high; fd++) close(fd);
}

/* Closes every descriptor above 2 but [k]. */
static void close_from_3(int k) {
#if defined(__APPLE__)
  struct proc_fdinfo fds[4096];
  int n = proc_pidinfo(getpid(), PROC_PIDLISTFDS, 0, fds, sizeof fds);
  if (n > 0 && n < (int)sizeof fds) {
    for (int i = 0; i < n / (int)PROC_PIDLISTFD_SIZE; i++)
      if (fds[i].proc_fd > 2 && fds[i].proc_fd != k) close(fds[i].proc_fd);
    return;
  }
#elif defined(__linux__) && defined(SYS_close_range)
  if (k <= 2) {
    if (syscall(SYS_close_range, 3, ~0U, 0) == 0) return;
  } else if (syscall(SYS_close_range, 3, k - 1, 0) == 0 &&
             syscall(SYS_close_range, k + 1, ~0U, 0) == 0)
    return;
#elif defined(__FreeBSD__)
  if (k <= 2) {
    if (close_range(3, ~0U, 0) == 0) return;
  } else if (close_range(3, k - 1, 0) == 0 && close_range(k + 1, ~0U, 0) == 0)
    return;
#endif
  long max = sysconf(_SC_OPEN_MAX);
  if (max < 0 || max > 1 << 20) max = 1 << 20;
  if (k > 2) {
    close_range_from(3, k);
    close_range_from(k + 1, (int)max);
  } else
    close_range_from(3, (int)max);
}

static const char *steps[] = {"setsid", "ioctl(TIOCSCTTY)", "dup2",
                              "chdir",  "fork",             "setpgid",
                              "tcsetpgrp", "execve"};

#define STEPS (int)(sizeof steps / sizeof *steps)

/* A failed [step], or the program's [pid] when [step] is -1. */
struct report {
  int err;
  int step;
  int pid;
};

static void report(int out, struct report r) {
  while (write(out, &r, sizeof r) == -1 && errno == EINTR)
    ;
}

/* Leads the session until [prog] has exited and the master has read what it
   wrote: when a session leader exits, FreeBSD revokes its terminal and drops
   what is unread, whoever else holds the slave. Exits as [prog] did. */
static void keep(pid_t prog) {
  static const int terminal[] = {SIGHUP,  SIGINT,  SIGQUIT,
                                 SIGTSTP, SIGTTIN, SIGTTOU};
  int st;
  for (size_t i = 0; i < sizeof terminal / sizeof *terminal; i++)
    signal(terminal[i], SIG_IGN);
  while (waitpid(prog, &st, 0) == -1)
    if (errno != EINTR) _exit(127);
  while (tcdrain(0) == -1 && errno == EINTR)
    ;
  if (WIFSIGNALED(st)) {
    struct rlimit no_core = {0, 0};
    setrlimit(RLIMIT_CORE, &no_core);
    signal(WTERMSIG(st), SIG_DFL);
    kill(getpid(), WTERMSIG(st));
  }
  _exit(WIFEXITED(st) ? WEXITSTATUS(st) : 127);
}

/* The child of [native_spawn], which keeps the session for the program it
   forks: only async-signal-safe calls until exec, as the OCaml runtime is not
   usable there. */
static void child(int fd, int out, const char *path, char **argv, char **sh,
                  char **env, const char *cwd) {
  struct report r = {0, 0, 0};
  sigset_t none, ttou;
  for (int s = 1; s < NSIG; s++)
    if (s != SIGKILL && s != SIGSTOP) signal(s, SIG_DFL);
  sigemptyset(&none);
  sigprocmask(SIG_SETMASK, &none, NULL);
  if (setsid() == -1) goto fail;
  r.step++;
  if (ioctl(fd, TIOCSCTTY, 0) == -1) goto fail;
  r.step++;
  if (out <= 2 && (out = fcntl(out, F_DUPFD_CLOEXEC, 3)) == -1) goto fail;
  /* dup2 onto itself keeps close-on-exec. */
  for (int i = 0; i <= 2; i++)
    if (i == fd ? fcntl(i, F_SETFD, 0) == -1 : dup2(fd, i) == -1) goto fail;
  close_from_3(out);
  r.step++;
  if (chdir(cwd) == -1) goto fail;
  r.step++;
  pid_t prog = fork();
  if (prog == -1) goto fail;
  if (prog > 0) {
    report(out, (struct report){0, -1, prog});
    close(out);
    keep(prog);
  }
  /* The program leads the terminal's foreground group, which is the group
     k7 signals. */
  r.step++;
  if (setpgid(0, 0) == -1) goto fail;
  r.step++;
  sigemptyset(&ttou);
  sigaddset(&ttou, SIGTTOU);
  sigprocmask(SIG_BLOCK, &ttou, NULL);
  if (tcsetpgrp(0, getpid()) == -1) goto fail;
  sigprocmask(SIG_SETMASK, &none, NULL);
  r.step++;
  execve(path, argv, env);
  /* As execvp, a file without a #! line is a shell script. */
  if (errno == ENOEXEC) execve("/bin/sh", sh, env);
fail:
  r.err = errno;
  report(out, r);
  _exit(127);
}

static void set_cloexec(int fd) { fcntl(fd, F_SETFD, FD_CLOEXEC); }
#endif

/* Runs [path] with [argv] and [env] in [cwd], in the foreground group of a
   new session whose controlling terminal is the pty slave [fd], and is
   [Ok (keeper, group)], or [Error (e, f)] when the call [f] failed with [e] in
   the child. [keeper] leads the session and exits as the program does once its
   output is read; [group] is the program's pid and process group. [fork]
   happens in C because [Unix.fork] fails once the process has spawned a
   domain. */
CAMLprim value native_spawn(value fd, value path, value argv, value env,
                            value cwd) {
  CAMLparam5(fd, path, argv, env, cwd);
  CAMLlocal3(res, err, pids);
#ifdef _WIN32
  caml_unix_error(ENOSYS, "spawn", Nothing);
#else
  caml_unix_check_path(path, "execve");
  caml_unix_check_path(cwd, "chdir");
  for (mlsize_t i = 0; i < Wosize_val(env); i++)
    if (!caml_string_is_c_safe(Field(env, i)))
      caml_unix_error(EINVAL, "execve", Nothing);
  char **c_argv = caml_unix_cstringvect(argv, "execve");
  char **c_env = caml_unix_cstringvect(env, "execve");
  char *c_path = caml_stat_strdup(String_val(path));
  char *c_cwd = caml_stat_strdup(String_val(cwd));
  int argc = 0;
  while (c_argv[argc] != NULL) argc++;
  char **sh = caml_stat_alloc((argc + 2) * sizeof(char *));
  sh[0] = "/bin/sh";
  sh[1] = c_path;
  for (int i = 1; i <= argc; i++) sh[i + 1] = c_argv[i];
  struct report r = {0, -1, 0};
  int pipe_fds[2], pid = -1, prog = 0, saved = 0;
  if (pipe(pipe_fds) == -1) {
    saved = errno;
    r.step = -2;
  } else {
    set_cloexec(pipe_fds[0]);
    set_cloexec(pipe_fds[1]);
    sigset_t all, old;
    sigfillset(&all);
    pthread_sigmask(SIG_SETMASK, &all, &old);
    pid = fork();
    if (pid == 0)
      child(Int_val(fd), pipe_fds[1], c_path, c_argv, sh, c_env, c_cwd);
    saved = errno;
    pthread_sigmask(SIG_SETMASK, &old, NULL);
    close(pipe_fds[1]);
    if (pid > 0) {
      struct report rs[2];
      caml_enter_blocking_section();
      size_t got = 0;
      while (got < sizeof rs) {
        ssize_t n = read(pipe_fds[0], (char *)rs + got, sizeof rs - got);
        if (n > 0)
          got += n;
        else if (n == 0 || errno != EINTR)
          break;
      }
      /* The keeper reports the program, which reports a step that failed. */
      for (size_t i = 0; i < got / sizeof *rs; i++)
        if (rs[i].step == -1)
          prog = rs[i].pid;
        else
          r = rs[i];
      if (got % sizeof *rs != 0 || r.step < -1 || r.step >= STEPS ||
          (r.step == -1 && prog <= 0)) {
        r.err = EIO;
        r.step = 4;
      }
      if (r.step != -1)
        while (waitpid(pid, NULL, 0) == -1 && errno == EINTR)
          ;
      caml_leave_blocking_section();
    }
    close(pipe_fds[0]);
  }
  caml_stat_free(sh);
  caml_unix_cstringvect_free(c_argv);
  caml_unix_cstringvect_free(c_env);
  caml_stat_free(c_path);
  caml_stat_free(c_cwd);
  if (r.step == -2) caml_unix_error(saved, "pipe", Nothing);
  if (pid < 0) caml_unix_error(saved, "fork", Nothing);
  if (r.step == -1) {
    pids = caml_alloc_tuple(2);
    Store_field(pids, 0, Val_int(pid));
    Store_field(pids, 1, Val_int(prog));
    res = caml_alloc(1, 0);
    Store_field(res, 0, pids);
  } else {
    err = caml_alloc_tuple(2);
    Store_field(err, 0, caml_unix_error_of_code(r.err));
    Store_field(err, 1, caml_copy_string(steps[r.step]));
    res = caml_alloc(1, 1);
    Store_field(res, 0, err);
  }
#endif
  CAMLreturn(res);
}
