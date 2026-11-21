// linux check Dockerfile or Dockerfile.musl
// osx   gcc terminal.c -I/opt/homebrew/include -L/opt/homebrew/lib -lvterm -o terminal.so -shared -fPIC

#ifdef __linux__
  #define _GNU_SOURCE  /* pipe2, ptsname_r */
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <sys/time.h>
#include <vterm.h>
#include <signal.h>
#include <pthread.h>

#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/resource.h>
#ifdef __linux__
  #include <sys/syscall.h>
  /* Headers older than Linux 5.9 (the Dockerfile's) lack it.  436 is its
     number in the generic syscall table these architectures use (not on
     alpha, ia64 or MIPS, which close one by one); an older kernel gives
     ENOSYS. */
  #if !defined(SYS_close_range) && (defined(__x86_64__) || defined(__i386__) \
      || defined(__aarch64__) || defined(__arm__) || defined(__riscv))
    #define SYS_close_range 436
  #endif
#endif
#include <termios.h>

#ifdef __APPLE__
  #include <libproc.h>
  #include <crt_externs.h>
#elif __FreeBSD__
  #include <sys/types.h>
  #include <sys/ioctl.h>
  #include <sys/sysctl.h>
  #include <sys/user.h>
  #include <termios.h>
#endif

///
typedef struct {
  pid_t pid;
  int fd;
} run_shell_result;

#ifdef __APPLE__
  #define ENVIRON (*_NSGetEnviron())
#else
  extern char **environ;
  #define ENVIRON environ
#endif

/* Open a PTY pair with both ends close-on-exec from the start, so that a fork
   in another thread (another terminal starting, uiop:run-program) cannot
   inherit them.  macOS's posix_openpt takes no O_CLOEXEC: there it is set
   right after, which leaves that race open for the master only. */
static int open_pty(int *master_out, int *slave_out, int rows, int cols)
{
#if defined(__APPLE__)
  int master = posix_openpt(O_RDWR | O_NOCTTY);
  if (master >= 0)
    fcntl(master, F_SETFD, FD_CLOEXEC);
#else
  int master = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
#endif
  if (master < 0)
    return -1;
  char name[128];
  int ok = grantpt(master) == 0 && unlockpt(master) == 0;
#ifdef __linux__
  ok = ok && ptsname_r(master, name, sizeof(name)) == 0;
#else
  /* ptsname's buffer is static; terminals are started from one thread. */
  const char *p = ok ? ptsname(master) : NULL;
  ok = p != NULL && strlen(p) < sizeof(name);
  if (ok)
    strcpy(name, p);
#endif
  int slave = ok ? open(name, O_RDWR | O_NOCTTY | O_CLOEXEC) : -1;
  if (slave < 0) {
    int saved = errno;
    close(master);
    errno = saved;
    return -1;
  }
  struct winsize win = { rows, cols, 0, 0 };
  ioctl(slave, TIOCSWINSZ, &win);
  *master_out = master;
  *slave_out = slave;
  return 0;
}

/* The path execvp would run for PROGRAM, looked up in the parent so the child
   only has to call execve.  NULL when it is not found, with *ERR telling why:
   EACCES when a file was found but cannot be executed (as execvp reports),
   ENOENT when there is none, ENOMEM. */
static char *find_program(const char *program, int *err)
{
  *err = ENOMEM;
  if (strchr(program, '/'))
    return strdup(program);
  const char *path = getenv("PATH");
  if (path == NULL)
    path = "/bin:/usr/bin";
  size_t len = strlen(program);
  *err = ENOENT;
  for (const char *dir = path; ; ) {
    const char *end = strchr(dir, ':');
    size_t dlen = end ? (size_t)(end - dir) : strlen(dir);
    char *candidate = malloc(dlen + len + 3);
    if (candidate == NULL) {
      *err = ENOMEM;
      return NULL;
    }
    if (dlen == 0) {
      strcpy(candidate, "./");
    } else {
      memcpy(candidate, dir, dlen);
      candidate[dlen] = '/';
      candidate[dlen + 1] = '\0';
    }
    strcat(candidate, program);
    struct stat st;
    if (stat(candidate, &st) == 0 && S_ISREG(st.st_mode)) {
      if (access(candidate, X_OK) == 0)
        return candidate;
      *err = EACCES;
    }
    free(candidate);
    if (end == NULL)
      return NULL;
    dir = end + 1;
  }
}

/* The environment of the shell: ours with TERM=xterm-256color.  It reads
   environ without the lock setenv takes, as getenv does: a thread adding a
   variable at the same time could move it. */
static char **make_child_environment(void)
{
  static char term[] = "TERM=xterm-256color";
  size_t n = 0;
  /* clearenv leaves it NULL */
  while (ENVIRON != NULL && ENVIRON[n] != NULL)
    n++;
  char **envp = malloc((n + 2) * sizeof(char *));
  if (envp == NULL)
    return NULL;
  size_t k = 0;
  for (size_t i = 0; i < n; i++)
    if (strncmp(ENVIRON[i], "TERM=", 5) != 0)
      envp[k++] = ENVIRON[i];
  envp[k++] = term;
  envp[k] = NULL;
  return envp;
}

/* ARGV for running PATH with /bin/sh, as execvp does for a file that is not
   an executable format (a script without #!). */
static char **make_sh_argv(char *path, char *const argv[])
{
  size_t n = 0;
  while (argv[n] != NULL)
    n++;
  char **sh_argv = malloc((n + 3) * sizeof(char *));
  if (sh_argv == NULL)
    return NULL;
  static char sh[] = "/bin/sh";
  sh_argv[0] = sh;
  sh_argv[1] = path;
  for (size_t i = 1; i < n; i++)
    sh_argv[i + 1] = argv[i];
  sh_argv[n > 0 ? n + 1 : 2] = NULL;
  return sh_argv;
}

/* Close every descriptor above stderr in the child: Lem's own files and
   sockets are not opened close-on-exec, and the shell would keep them (a log
   file, a listening socket) for as long as it runs.  MAX_FD comes from the
   parent, as getrlimit is not async-signal-safe. */
static void close_other_fds(int max_fd)
{
#if defined(__linux__) && defined(SYS_close_range)
  if (syscall(SYS_close_range, 3U, ~0U, 0U) == 0)
    return;
#elif defined(__FreeBSD__)
  closefrom(3);
  return;
#endif
  for (int fd = 3; fd < max_fd; fd++)
    close(fd);
}

/* The highest descriptor number the child may have to close where it closes
   them one by one (macOS, Linux before 5.9).  Capped, as the soft limit can
   be a million: descriptors above the cap are left open there. */
static int fd_limit(void)
{
  struct rlimit rl;
  if (getrlimit(RLIMIT_NOFILE, &rl) != 0 || rl.rlim_cur == RLIM_INFINITY
      || rl.rlim_cur > 65536)
    return 65536;
  return (int) rl.rlim_cur;
}

/* Text for the errors execve can give; strerror is not async-signal-safe. */
static const char *exec_error_text(int err)
{
  switch (err) {
  case ENOENT: return "No such file or directory";
  case EACCES: return "Permission denied";
  case ENOEXEC: return "Exec format error";
  case ENOTDIR: return "Not a directory";
  case ELOOP: return "Too many levels of symbolic links";
  case ENAMETOOLONG: return "File name too long";
  case E2BIG: return "Argument list too long";
  case ENOMEM: return "Cannot allocate memory";
  case ETXTBSY: return "Text file busy";
  case EISDIR: return "Is a directory";
  default: return "cannot be executed";
  }
}

static void write_all(int fd, const char *s)
{
  size_t len = strlen(s);
  while (len > 0) {
    ssize_t r = write(fd, s, len);
    if (r < 0 && errno == EINTR)
      continue;
    if (r <= 0)
      return;
    s += r;
    len -= (size_t)r;
  }
}

/* Start PROGRAM with ARGV on a new PTY.  Everything the child needs is made
   here before the fork: the child of a multithreaded process (SBCL) may only
   call async-signal-safe functions, as another thread may have held the
   malloc, stdio or environment lock when it forked.  On failure pid is -1
   and errno tells why. */
static run_shell_result run_shell(int rows, int cols, const char *program, char* const argv[])
{
  run_shell_result result = { -1, -1 };
  int master, slave;
  if (open_pty(&master, &slave, rows, cols) != 0)
    return result;

  int lookup_error;
  char *path = find_program(program, &lookup_error);
  char **envp = make_child_environment();
  char **sh_argv = path ? make_sh_argv(path, argv) : NULL;
  if (envp == NULL || (path != NULL && sh_argv == NULL)
      || (path == NULL && lookup_error == ENOMEM)) {
    free(envp);
    free(sh_argv);
    free(path);
    close(master);
    close(slave);
    errno = ENOMEM;
    return result;
  }

  int max_fd = fd_limit();
  /* No handler of the editor may run in the child before its signals are
     reset: block them all across the fork. */
  sigset_t all, old_mask;
  sigfillset(&all);
  pthread_sigmask(SIG_SETMASK, &all, &old_mask);
  pid_t pid = fork();
  if (pid == 0) {
    /* The child: only async-signal-safe calls from here on. */
    setsid();
    if (ioctl(slave, TIOCSCTTY, 0) != 0) {
      /* Without a controlling terminal there is no job control (C-c). */
      write_all(slave, "lem-terminal: cannot make the PTY the controlling terminal\n");
      _exit(1);
    }
    dup2(slave, 0);
    dup2(slave, 1);
    dup2(slave, 2);
    /* dup2 onto itself keeps close-on-exec: clear it when the slave is one
       of 0-2 already. */
    if (slave <= 2)
      fcntl(slave, F_SETFD, 0);
    /* Exec resets caught signals but keeps ignored ones (SBCL ignores
       SIGPIPE) and the mask: reset both, the mask last. */
    for (int sig = 1; sig < NSIG; sig++)
      if (sig != SIGKILL && sig != SIGSTOP)
        signal(sig, SIG_DFL);
    sigset_t none;
    sigemptyset(&none);
    sigprocmask(SIG_SETMASK, &none, NULL);
    close_other_fds(max_fd);
    int err = lookup_error;
    if (path != NULL) {
      execve(path, argv, envp);
      err = errno;
      if (err == ENOEXEC) {
        execve(sh_argv[0], sh_argv, envp);
        err = errno;
      }
    }
    /* Only reached when exec failed.  Report it on the PTY so the user sees
       why the terminal died, and exit with the shell convention for
       "command not found" instead of aborting (which would dump core). */
    write_all(2, "lem-terminal: cannot execute ");
    write_all(2, program);
    write_all(2, ": ");
    write_all(2, exec_error_text(err));
    write_all(2, "\n");
    _exit(127);
  }

  int saved = errno;
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
  free(path);
  free(envp);
  free(sh_argv);
  close(slave);
  if (pid < 0) {
    /* Out of processes: let terminal_new fail instead of aborting the
       editor. */
    close(master);
    errno = saved;
    return result;
  }
  result.pid = pid;
  result.fd = master;
  return result;
}

///
struct terminal {
  int id;
  VTerm *vterm;
  VTermScreen *screen;
  int fd;
  int pid;

  /* Process lifecycle.
     wake_pipe: written by terminal_wake to interrupt the I/O thread's
     select so it can exit without being killed asynchronously.
     eof: set once the PTY reports hangup (the child side is closed).
     reaped/exit_status: set once waitpid has collected the child. */
  int wake_pipe[2];
  int eof;
  int reaped;
  int exit_status;

  VTermScreenCell lastCell;
  VTermPos cursorPos;
  int cursorVisible;

  int (*cb_damage)(VTermRect *rect, int id);
  int (*cb_moverect)(VTermRect *dest, VTermRect *src, int id);
  int (*cb_movecursor)(VTermPos *pos, VTermPos *oldpos, int visible, int id);
  int (*cb_settermprop)(VTermProp prop, VTermValue *val, int id);
  int (*cb_bell)(int id);
  int (*cb_resize)(int rows, int cols, int id);
  int (*cb_sb_pushline)(int cols, const VTermScreenCell *cells, int id);
  int (*cb_sb_popline)(int cols, VTermScreenCell *cells, int id);
  int (*cb_sb_clear)(int id);

  /* Per-terminal scrollback line extraction buffer.
     Max line: 1024 cols * 4 bytes UTF-8 + NUL = 4097 bytes.
     Per-terminal (not file-scope static) so concurrent terminals
     don't race when their I/O threads invoke vterm callbacks. */
  char sb_line_buf[4097];
  int  sb_line_len;

  /* Window title (OSC 0/2), assembled from vterm's string fragments. */
  char   title[1024];
  size_t title_len;
};

static int cb_damage(VTermRect rect, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_damage(&rect, terminal->id);
  return 0;
}

static int cb_moverect(VTermRect dest, VTermRect src, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_moverect(&dest, &src, terminal->id);
  return 0;
}

static int cb_movecursor(VTermPos pos, VTermPos oldpos, int visible,
			 void *user)
{
  struct terminal *terminal = ((struct terminal *)user);

  // TODO
  terminal->cursorPos = pos;
  terminal->cursorVisible = visible;

  terminal->cb_movecursor(&pos, &oldpos, visible, terminal->id);
  return 0;
}

static int cb_settermprop(VTermProp prop, VTermValue *val, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  if (prop == VTERM_PROP_TITLE) {
    /* The title may arrive in several fragments; report it once complete.
       Overlong titles are truncated. */
    if (val->string.initial)
      terminal->title_len = 0;
    size_t room = sizeof(terminal->title) - 1 - terminal->title_len;
    size_t n = val->string.len < room ? val->string.len : room;
    memcpy(terminal->title + terminal->title_len, val->string.str, n);
    terminal->title_len += n;
    terminal->title[terminal->title_len] = '\0';
    if (!val->string.final)
      return 1;
  }
  terminal->cb_settermprop(prop, val, terminal->id);
  return 0;
}

static int cb_bell(void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_bell(terminal->id);
  return 0;
}

static int cb_resize(int rows, int cols, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_resize(rows, cols, terminal->id);
  return 0;
}

static int cb_sb_pushline(int cols, const VTermScreenCell *cells,
			  void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_sb_pushline(cols, cells, terminal->id);
  return 0;
}

static int cb_sb_popline(int cols, VTermScreenCell *cells, void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_sb_popline(cols, cells, terminal->id);
  return 0;
}

/* ESC[3J (sent by `clear` and `reset`) discards the scrollback. */
static int cb_sb_clear(void *user)
{
  struct terminal *terminal = ((struct terminal *)user);
  terminal->cb_sb_clear(terminal->id);
  return 0;
}

static void output_callback(const char *s, size_t len, void *user)
{
  struct terminal *terminal = (struct terminal *) user;
  write(terminal->fd, s, len);
}

VTermScreenCallbacks screen_callbacks = {
  cb_damage,
  cb_moverect,
  cb_movecursor,
  cb_settermprop,
  cb_bell,
  cb_resize,
  cb_sb_pushline,
  cb_sb_popline,
  cb_sb_clear,
};

/* errno of this thread's last failed terminal_new.  Kept apart from errno,
   which the FFI's own frees and the Lisp runtime can change before the
   caller reads it. */
static _Thread_local int last_error;

/* Why the last terminal_new of this thread returned NULL (an errno value). */
int terminal_last_error(void)
{
  return last_error;
}

struct terminal *terminal_new(int id,
                              int rows,
                              int cols,
                              const char *program,
                              char* const argv[],
			      void *cb_damage,
			      void *cb_moverect,
			      void *cb_movecursor,
			      void *cb_settermprop,
			      void *cb_bell,
			      void *cb_resize,
			      void *cb_sb_pushline,
                              void *cb_sb_popline,
                              void *cb_sb_clear)
{
  /* Everything that can fail is done before the shell is started, so a
     failure has no child to kill and reap and no PTY to close.  On failure
     NULL is returned, and terminal_last_error tells why. */
  VTerm *vterm = NULL;

  /* Zeroed: the cursor is read at the first render, which can come before
     libvterm reports where the cursor is. */
  struct terminal *terminal = calloc(1, sizeof(struct terminal));
  if (terminal == NULL) {
    last_error = ENOMEM;
    return NULL;
  }
  /* Without the wake pipe the I/O thread could not be woken to exit. */
#ifdef __APPLE__
  /* No pipe2: FD_CLOEXEC is set right after (see open_pty). */
  if (pipe(terminal->wake_pipe) != 0) {
#else
  if (pipe2(terminal->wake_pipe, O_CLOEXEC) != 0) {
#endif
    last_error = errno;
    free(terminal);
    return NULL;
  }
#ifdef __APPLE__
  fcntl(terminal->wake_pipe[0], F_SETFD, FD_CLOEXEC);
  fcntl(terminal->wake_pipe[1], F_SETFD, FD_CLOEXEC);
#endif

  vterm = vterm_new(rows, cols);
  if (vterm == NULL) {
    last_error = ENOMEM;
    goto fail;
  }
  vterm_set_utf8(vterm, 1);

  VTermScreen *screen = vterm_obtain_screen(vterm);
  if (screen == NULL) {
    last_error = ENOMEM;
    goto fail;
  }
  /* Full-screen programs (less, vi, top, ...) switch to the alternate screen
     and expect the original one back on exit; without this their last frame
     overwrote the shell output and leaked into the scrollback. */
  vterm_screen_enable_altscreen(screen, 1);

  run_shell_result result = run_shell(rows, cols, program, argv);
  if (result.pid < 0) {
    last_error = errno;
    goto fail;
  }

  terminal->id = id;
  terminal->screen = screen;
  terminal->vterm = vterm;
  terminal->fd = result.fd;
  terminal->pid = result.pid;
  terminal->eof = 0;
  terminal->reaped = 0;
  terminal->exit_status = -1;

  terminal->cb_damage = cb_damage;
  terminal->cb_moverect = cb_moverect;
  terminal->cb_movecursor = cb_movecursor;
  terminal->cb_settermprop = cb_settermprop;
  terminal->cb_bell = cb_bell;
  terminal->cb_resize = cb_resize;
  terminal->cb_sb_pushline = cb_sb_pushline;
  terminal->cb_sb_popline = cb_sb_popline;
  terminal->cb_sb_clear = cb_sb_clear;
  terminal->sb_line_buf[0] = '\0';
  terminal->sb_line_len = 0;
  terminal->title[0] = '\0';
  terminal->title_len = 0;

  vterm_output_set_callback(vterm, output_callback, (void *) terminal);
  vterm_screen_set_callbacks(screen, &screen_callbacks, terminal);
  vterm_screen_reset(screen, 1);

  return terminal;

 fail:
  if (vterm != NULL)
    vterm_free(vterm);
  close(terminal->wake_pipe[0]);
  close(terminal->wake_pipe[1]);
  free(terminal);
  return NULL;
}

static int decode_wait_status(int status)
{
  if (WIFEXITED(status))
    return WEXITSTATUS(status);
  if (WIFSIGNALED(status))
    return 128 + WTERMSIG(status);
  return -1;
}

/* Try to collect the child without blocking.  Returns 1 once the child has
   been reaped (now or earlier), 0 while it is still running. */
static int try_reap(struct terminal *terminal)
{
  if (terminal->reaped)
    return 1;
  int status;
  pid_t r = waitpid(terminal->pid, &status, WNOHANG);
  if (r == terminal->pid) {
    terminal->reaped = 1;
    terminal->exit_status = decode_wait_status(status);
    return 1;
  }
  if (r < 0 && errno == ECHILD) {
    /* Someone else (e.g. SIGCHLD set to SIG_IGN) already collected it. */
    terminal->reaped = 1;
    terminal->exit_status = -1;
    return 1;
  }
  return 0;
}

/* Reap the child, waiting up to MAX_MS for it to exit. */
static int reap_with_timeout(struct terminal *terminal, int max_ms)
{
  for (int waited = 0; ; waited += 10) {
    if (try_reap(terminal))
      return 1;
    if (waited >= max_ms)
      return 0;
    usleep(10 * 1000);
  }
}

/* Returns the last complete window title set by the program (OSC 0/2). */
const char *terminal_title(struct terminal *terminal)
{
  return terminal->title;
}

/* Copy the child's current working directory into BUF (NUL-terminated) and
   return its length, or -1 when it cannot be determined. */
int terminal_cwd(struct terminal *terminal, char *buf, int size)
{
  if (size <= 0 || terminal->pid <= 0)
    return -1;
#if defined(__APPLE__)
  struct proc_vnodepathinfo info;
  if (proc_pidinfo(terminal->pid, PROC_PIDVNODEPATHINFO, 0,
                   &info, sizeof(info)) != (int)sizeof(info))
    return -1;
  int len = (int)strlen(info.pvi_cdir.vip_path);
  if (len >= size)
    return -1;
  memcpy(buf, info.pvi_cdir.vip_path, len + 1);
  return len;
#elif defined(__FreeBSD__)
  int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_CWD, terminal->pid };
  struct kinfo_file info;
  size_t info_size = sizeof(info);
  memset(&info, 0, sizeof(info));
  info.kf_structsize = sizeof(info);
  if (sysctl(mib, 4, &info, &info_size, NULL, 0) != 0)
    return -1;
  int len = (int)strlen(info.kf_path);
  if (len == 0 || len >= size)
    return -1;
  memcpy(buf, info.kf_path, len + 1);
  return len;
#else
  char link[64];
  snprintf(link, sizeof(link), "/proc/%d/cwd", (int)terminal->pid);
  ssize_t len = readlink(link, buf, size - 1);
  if (len < 0)
    return -1;
  buf[len] = '\0';
  return (int)len;
#endif
}

/* Returns the process id of the child (the shell). */
int terminal_pid(struct terminal *terminal)
{
  return terminal->pid;
}

/* Returns 1 once the PTY has hung up (the child closed its side). */
int terminal_eof_p(struct terminal *terminal)
{
  return terminal->eof;
}

/* Called by the I/O thread after it observed EOF.  Gives the child a short
   grace period to finish exiting, then returns its exit status: the exit
   code, 128+signal when killed by a signal, or -1 when unknown (not yet
   exited, or reaped elsewhere). */
int terminal_collect_exit_status(struct terminal *terminal)
{
  reap_with_timeout(terminal, 500);
  return terminal->reaped ? terminal->exit_status : -1;
}

/* Interrupt a blocked terminal_process_input_wait so the I/O thread can
   observe that the terminal is being destroyed. */
void terminal_wake(struct terminal *terminal)
{
  if (terminal->wake_pipe[1] >= 0) {
    char c = 0;
    ssize_t r;
    do {
      r = write(terminal->wake_pipe[1], &c, 1);
    } while (r < 0 && errno == EINTR);
  }
}

/* Free the terminal and make sure the child process does not outlive it.
   The I/O thread must already have stopped using TERMINAL. */
void terminal_delete(struct terminal *terminal)
{
  if (!try_reap(terminal)) {
    /* run_shell made the child a session leader, so -pid addresses its whole
       process group (the shell and its foreground job). */
    kill(-terminal->pid, SIGHUP);
    kill(terminal->pid, SIGHUP);
  }
  close(terminal->fd);
  if (!reap_with_timeout(terminal, 200)) {
    kill(-terminal->pid, SIGKILL);
    kill(terminal->pid, SIGKILL);
    waitpid(terminal->pid, NULL, 0);
  }
  if (terminal->wake_pipe[0] >= 0)
    close(terminal->wake_pipe[0]);
  if (terminal->wake_pipe[1] >= 0)
    close(terminal->wake_pipe[1]);
  vterm_free(terminal->vterm);
  free(terminal);
}

void terminal_input_char(struct terminal *terminal, uint32_t c,
			 VTermModifier mod)
{
  vterm_keyboard_unichar(terminal->vterm, c, mod);
}

void terminal_input_key(struct terminal *terminal, VTermKey key,
			VTermModifier mod)
{
  vterm_keyboard_key(terminal->vterm, key, mod);
}

/* Bracket pasted text.  vterm emits ESC[200~ / ESC[201~ only when the
   program has enabled bracketed paste mode (DECSET 2004). */
void terminal_paste_start(struct terminal *terminal)
{
  vterm_keyboard_start_paste(terminal->vterm);
}

void terminal_paste_end(struct terminal *terminal)
{
  vterm_keyboard_end_paste(terminal->vterm);
}

/* Block until the PTY has data (or has hung up) or terminal_wake was
   called.  Returns 1 when the PTY is readable, 0 when woken (or the PTY
   already hit EOF, so there is nothing left to wait for). */
int terminal_process_input_wait(struct terminal *terminal)
{
  int fd = terminal->fd;
  int wfd = terminal->wake_pipe[0];
  if (terminal->eof)
    return 0;
  for (;;) {
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    if (wfd >= 0)
      FD_SET(wfd, &readfds);
    int nfds = (wfd > fd ? wfd : fd) + 1;
    int r = select(nfds, &readfds, NULL, NULL, NULL);
    if (r < 0) {
      if (errno == EINTR)
        continue;
      return 0;
    }
    if (wfd >= 0 && FD_ISSET(wfd, &readfds))
      return 0;
    return 1;
  }
}

/* Time-budgeted PTY drain (xterm.js WriteBuffer pattern).
   Reads and parses PTY data in a loop until either:
   - the fd would block (no more data), or
   - max_ms milliseconds have elapsed since the call started.
   Uses a 64KB read buffer.  Returns total bytes processed.
   Pass max_ms=0 to drain fully without time limit. */
size_t terminal_process_input_timed(struct terminal *terminal, int max_ms)
{
  int fd = terminal->fd;
  char buf[65536];
  size_t total = 0;
  struct timeval start;
  gettimeofday(&start, NULL);

  for (;;) {
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    struct timeval timeout = { 0, 0 };
    if (select(fd + 1, &readfds, NULL, NULL, &timeout) <= 0)
      break;
    ssize_t size = read(fd, buf, sizeof(buf));
    if (size < 0 && (errno == EINTR || errno == EAGAIN))
      break;
    if (size <= 0) {
      /* Linux reports a hung-up PTY master as EIO, BSD/macOS as EOF (0).
         Either way the child side is gone; remember it so the I/O thread
         stops instead of spinning on a permanently-readable fd. */
      terminal->eof = 1;
      break;
    }
    vterm_input_write(terminal->vterm, buf, size);
    total += size;

    /* Check time budget */
    if (max_ms > 0) {
      struct timeval now;
      gettimeofday(&now, NULL);
      long elapsed_ms = (now.tv_sec - start.tv_sec) * 1000
                      + (now.tv_usec - start.tv_usec) / 1000;
      if (elapsed_ms >= max_ms)
        break;
    } else {
      /* max_ms == 0: single-shot drain, but still loop to empty the fd */
    }
  }
  return total;
}

size_t terminal_process_input_nonblock(struct terminal *terminal)
{
  return terminal_process_input_timed(terminal, 0);
}

void terminal_process_input(struct terminal *terminal)
{
  int fd = terminal->fd;
  char buf[4096];
  ssize_t size = read(fd, buf, sizeof(buf));
  if (size > 0) {
    vterm_input_write(terminal->vterm, buf, size);
  }
}

VTermScreenCell *terminal_query_cell(struct terminal *terminal, int x,
				     int y)
{
  VTermPos pos = { .row = y, .col = x };
  VTermScreenCell cell;
  vterm_screen_get_cell(terminal->screen, pos, &cell);
  if (VTERM_COLOR_IS_INDEXED(&cell.fg)) {
    vterm_screen_convert_color_to_rgb(terminal->screen, &cell.fg);
  }
  if (VTERM_COLOR_IS_INDEXED(&cell.bg)) {
    vterm_screen_convert_color_to_rgb(terminal->screen, &cell.bg);
  }
  terminal->lastCell = cell;
  return &terminal->lastCell;
}

/* Packed cell data for bulk row transfer - 12 bytes per cell */
struct cell_data {
  uint32_t ch;          /* first character code (0 if empty) */
  uint8_t  fg_r, fg_g, fg_b;
  uint8_t  bg_r, bg_g, bg_b;
  uint8_t  bold;
  uint8_t  underline;
  uint8_t  reverse_attr;
  uint8_t  width;
  uint8_t  default_fg;  /* 1 when fg is the terminal default colour */
  uint8_t  default_bg;  /* 1 when bg is the terminal default colour */
};

/* Fill 'out' with packed cell data for an entire row.
   Caller must provide a buffer of at least 'cols' cell_data structs.
   Returns the number of cells written (== cols). */
int terminal_get_row(struct terminal *terminal, int row,
                     struct cell_data *out, int cols)
{
  VTermScreen *screen = terminal->screen;
  for (int col = 0; col < cols; col++) {
    VTermPos pos = { .row = row, .col = col };
    VTermScreenCell cell;
    vterm_screen_get_cell(screen, pos, &cell);
    out[col].default_fg  = VTERM_COLOR_IS_DEFAULT_FG(&cell.fg) ? 1 : 0;
    out[col].default_bg  = VTERM_COLOR_IS_DEFAULT_BG(&cell.bg) ? 1 : 0;
    if (VTERM_COLOR_IS_INDEXED(&cell.fg)) {
      vterm_screen_convert_color_to_rgb(screen, &cell.fg);
    }
    if (VTERM_COLOR_IS_INDEXED(&cell.bg)) {
      vterm_screen_convert_color_to_rgb(screen, &cell.bg);
    }
    out[col].ch          = cell.chars[0];
    out[col].fg_r        = cell.fg.rgb.red;
    out[col].fg_g        = cell.fg.rgb.green;
    out[col].fg_b        = cell.fg.rgb.blue;
    out[col].bg_r        = cell.bg.rgb.red;
    out[col].bg_g        = cell.bg.rgb.green;
    out[col].bg_b        = cell.bg.rgb.blue;
    out[col].bold        = cell.attrs.bold;
    out[col].underline   = cell.attrs.underline;
    out[col].reverse_attr = cell.attrs.reverse;
    out[col].width       = cell.width;
  }
  return cols;
}

/* Extract text from a VTermScreenCell array into the terminal's per-terminal
   buffer.  Trailing ASCII spaces are stripped during extraction so the Lisp
   side does not need to allocate a trimmed copy.  Returns pointer to the
   buffer; caller must copy before the next call on the same terminal. */
const char *terminal_sb_line_extract(struct terminal *terminal,
                                     int cols,
                                     const VTermScreenCell *cells,
                                     int *out_len)
{
  char *buf = terminal->sb_line_buf;
  int pos = 0;
  int last_nonspace = 0;
  int max = (int)sizeof(terminal->sb_line_buf) - 1;
  for (int col = 0; col < cols; col++) {
    if (cells[col].width == 0) continue;
    uint32_t ch = cells[col].chars[0];
    if (ch == 0) ch = ' ';
    int start = pos;
    if (ch < 0x80) {
      if (pos + 1 > max) break;
      buf[pos++] = (char)ch;
    } else if (ch < 0x800) {
      if (pos + 2 > max) break;
      buf[pos++] = 0xC0 | (ch >> 6);
      buf[pos++] = 0x80 | (ch & 0x3F);
    } else if (ch < 0x10000) {
      if (pos + 3 > max) break;
      buf[pos++] = 0xE0 | (ch >> 12);
      buf[pos++] = 0x80 | ((ch >> 6) & 0x3F);
      buf[pos++] = 0x80 | (ch & 0x3F);
    } else {
      if (pos + 4 > max) break;
      buf[pos++] = 0xF0 | (ch >> 18);
      buf[pos++] = 0x80 | ((ch >> 12) & 0x3F);
      buf[pos++] = 0x80 | ((ch >> 6) & 0x3F);
      buf[pos++] = 0x80 | (ch & 0x3F);
    }
    if (ch != ' ') last_nonspace = pos;
    (void)start;
  }
  pos = last_nonspace;
  buf[pos] = '\0';
  terminal->sb_line_len = pos;
  if (out_len) *out_len = pos;
  return buf;
}

/* Legacy wrapper for compatibility */
int terminal_sb_line_to_string(struct terminal *terminal,
                               int cols,
                               const VTermScreenCell *cells,
                               char *out, int out_size)
{
  int len;
  terminal_sb_line_extract(terminal, cols, cells, &len);
  int copy = len < out_size ? len : out_size - 1;
  memcpy(out, terminal->sb_line_buf, copy);
  out[copy] = '\0';
  return copy;
}

uint32_t *terminal_last_cell_chars(struct terminal *terminal)
{
  return terminal->lastCell.chars;
}

int terminal_last_cell_width(struct terminal *terminal)
{
  return terminal->lastCell.width;
}

unsigned int terminal_last_cell_attrs_bold(struct terminal *terminal)
{
  return terminal->lastCell.attrs.bold;
}

unsigned int terminal_last_cell_attrs_underline(struct terminal *terminal)
{
  return terminal->lastCell.attrs.underline;
}

unsigned int terminal_last_cell_attrs_italic(struct terminal *terminal)
{
  return terminal->lastCell.attrs.italic;
}

unsigned int terminal_last_cell_attrs_blink(struct terminal *terminal)
{
  return terminal->lastCell.attrs.blink;
}

unsigned int terminal_last_cell_attrs_reverse(struct terminal *terminal)
{
  return terminal->lastCell.attrs.reverse;
}

unsigned int terminal_last_cell_attrs_conceal(struct terminal *terminal)
{
  return terminal->lastCell.attrs.conceal;
}

unsigned int terminal_last_cell_attrs_strike(struct terminal *terminal)
{
  return terminal->lastCell.attrs.strike;
}

unsigned int terminal_last_cell_attrs_font(struct terminal *terminal)
{
  return terminal->lastCell.attrs.font;
}

unsigned int terminal_last_cell_attrs_dwl(struct terminal *terminal)
{
  return terminal->lastCell.attrs.dwl;
}

unsigned int terminal_last_cell_attrs_dhl(struct terminal *terminal)
{
  return terminal->lastCell.attrs.dhl;
}

unsigned int terminal_last_cell_attrs_small(struct terminal *terminal)
{
  return terminal->lastCell.attrs.small;
}

unsigned int terminal_last_cell_attrs_baseline(struct terminal *terminal)
{
  return terminal->lastCell.attrs.baseline;
}

uint8_t terminal_last_cell_fg_red(struct terminal *terminal)
{
  return terminal->lastCell.fg.rgb.red;
}

uint8_t terminal_last_cell_fg_green(struct terminal *terminal)
{
  return terminal->lastCell.fg.rgb.green;
}

uint8_t terminal_last_cell_fg_blue(struct terminal *terminal)
{
  return terminal->lastCell.fg.rgb.blue;
}

uint8_t terminal_last_cell_bg_red(struct terminal *terminal)
{
  return terminal->lastCell.bg.rgb.red;
}

uint8_t terminal_last_cell_bg_green(struct terminal *terminal)
{
  return terminal->lastCell.bg.rgb.green;
}

uint8_t terminal_last_cell_bg_blue(struct terminal *terminal)
{
  return terminal->lastCell.bg.rgb.blue;
}

int terminal_cursor_row(struct terminal *terminal)
{
  return terminal->cursorPos.row;
}

int terminal_cursor_col(struct terminal *terminal)
{
  return terminal->cursorPos.col;
}

void terminal_resize(struct terminal *terminal, int rows, int cols)
{
  vterm_set_size(terminal->vterm, rows, cols);
  vterm_screen_flush_damage(terminal->screen);

  struct winsize win = { 0 };
  win.ws_row = rows;
  win.ws_col = cols;
  ioctl(terminal->fd, TIOCSWINSZ, &win);
}

#if 0
int main(void)
{
  int cols = 80, rows = 24;

  struct terminal *terminal = terminal_new(rows, cols);
  terminal_process_input(terminal);
  terminal_input_char(terminal, 'l', 0);
  terminal_process_input(terminal);
  terminal_input_char(terminal, 's', 0);
  terminal_process_input(terminal);
  terminal_input_key(terminal, VTERM_KEY_ENTER, 0);
  terminal_process_input(terminal);
  terminal_process_input(terminal);
  terminal_process_input(terminal);

  for (int row = 0; row < rows; row++) {
    for (int col = 0; col < cols; col++) {
      terminal_query_cell(terminal, col, row);
      printf("%c", terminal->lastCell.chars[0]);
    }
    putchar('\n');
  }
}
#endif

#if 0
int main(void)
{
  printf("VTERM_KEY_NONE: %d\n", VTERM_KEY_NONE);

  printf("VTERM_KEY_ENTER: %d\n", VTERM_KEY_ENTER);
  printf("VTERM_KEY_TAB: %d\n", VTERM_KEY_TAB);
  printf("VTERM_KEY_BACKSPACE: %d\n", VTERM_KEY_BACKSPACE);
  printf("VTERM_KEY_ESCAPE: %d\n", VTERM_KEY_ESCAPE);

  printf("VTERM_KEY_UP: %d\n", VTERM_KEY_UP);
  printf("VTERM_KEY_DOWN: %d\n", VTERM_KEY_DOWN);
  printf("VTERM_KEY_LEFT: %d\n", VTERM_KEY_LEFT);
  printf("VTERM_KEY_RIGHT: %d\n", VTERM_KEY_RIGHT);

  printf("VTERM_KEY_INS: %d\n", VTERM_KEY_INS);
  printf("VTERM_KEY_DEL: %d\n", VTERM_KEY_DEL);
  printf("VTERM_KEY_HOME: %d\n", VTERM_KEY_HOME);
  printf("VTERM_KEY_END: %d\n", VTERM_KEY_END);
  printf("VTERM_KEY_PAGEUP: %d\n", VTERM_KEY_PAGEUP);
  printf("VTERM_KEY_PAGEDOWN: %d\n", VTERM_KEY_PAGEDOWN);

  printf("VTERM_KEY_FUNCTION_0: %d\n", VTERM_KEY_FUNCTION_0);
  printf("VTERM_KEY_FUNCTION_MAX: %d\n", VTERM_KEY_FUNCTION_MAX);

  printf("VTERM_KEY_KP_0: %d\n", VTERM_KEY_KP_0);
  printf("VTERM_KEY_KP_1: %d\n", VTERM_KEY_KP_1);
  printf("VTERM_KEY_KP_2: %d\n", VTERM_KEY_KP_2);
  printf("VTERM_KEY_KP_3: %d\n", VTERM_KEY_KP_3);
  printf("VTERM_KEY_KP_4: %d\n", VTERM_KEY_KP_4);
  printf("VTERM_KEY_KP_5: %d\n", VTERM_KEY_KP_5);
  printf("VTERM_KEY_KP_6: %d\n", VTERM_KEY_KP_6);
  printf("VTERM_KEY_KP_7: %d\n", VTERM_KEY_KP_7);
  printf("VTERM_KEY_KP_8: %d\n", VTERM_KEY_KP_8);
  printf("VTERM_KEY_KP_9: %d\n", VTERM_KEY_KP_9);
  printf("VTERM_KEY_KP_MULT: %d\n", VTERM_KEY_KP_MULT);
  printf("VTERM_KEY_KP_PLUS: %d\n", VTERM_KEY_KP_PLUS);
  printf("VTERM_KEY_KP_COMMA: %d\n", VTERM_KEY_KP_COMMA);
  printf("VTERM_KEY_KP_MINUS: %d\n", VTERM_KEY_KP_MINUS);
  printf("VTERM_KEY_KP_PERIOD: %d\n", VTERM_KEY_KP_PERIOD);
  printf("VTERM_KEY_KP_DIVIDE: %d\n", VTERM_KEY_KP_DIVIDE);
  printf("VTERM_KEY_KP_ENTER: %d\n", VTERM_KEY_KP_ENTER);
  printf("VTERM_KEY_KP_EQUAL: %d\n", VTERM_KEY_KP_EQUAL);

  printf("VTERM_KEY_MAX: %d\n", VTERM_KEY_MAX);
  printf("VTERM_N_KEYS: %d\n", VTERM_N_KEYS);
}
#endif
