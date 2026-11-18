/* Smoke test for the lem-terminal native helper on each OS.
   Compiles terminal.c directly and drives it without Lisp. */
#include "../../extensions/terminal/terminal.c"
#include <limits.h>
#include <stdlib.h>

static int cb_rect(VTermRect *r, int id) { (void)r; (void)id; return 0; }
static int cb_moverect2(VTermRect *d, VTermRect *s, int id) { (void)d; (void)s; (void)id; return 0; }
static int cb_cursor(VTermPos *p, VTermPos *o, int v, int id) { (void)p; (void)o; (void)v; (void)id; return 0; }
static int cb_prop(VTermProp p, VTermValue *v, int id) { (void)p; (void)v; (void)id; return 0; }
static int cb_id(int id) { (void)id; return 0; }
static int cb_size(int r, int c, int id) { (void)r; (void)c; (void)id; return 0; }
static int cb_line(int c, const VTermScreenCell *cells, int id) { (void)c; (void)cells; (void)id; return 0; }
static int cb_line2(int c, VTermScreenCell *cells, int id) { (void)c; (void)cells; (void)id; return 0; }

static int failures = 0;
#define CHECK(cond, ...) do { if (cond) { printf("ok   "); } else { printf("FAIL "); failures++; } \
                              printf(__VA_ARGS__); putchar('\n'); } while (0)

static struct terminal *start(const char *script)
{
  char *argv[] = { "/bin/sh", "-c", (char *)script, NULL };
  return terminal_new(-1, 24, 80, "/bin/sh", argv,
                      cb_rect, cb_moverect2, cb_cursor, cb_prop, cb_id,
                      cb_size, cb_line, cb_line2, cb_id);
}

static void pump(struct terminal *t, int ms)
{
  for (int i = 0; i < ms / 10; i++) {
    terminal_process_input_timed(t, 10);
    usleep(10000);
  }
}

static void row_text(struct terminal *t, int row, char *out)
{
  struct cell_data cells[80];
  terminal_get_row(t, row, cells, 80);
  int n = 0;
  for (int c = 0; c < 80; c++)
    out[n++] = (cells[c].ch > 0 && cells[c].ch < 128) ? (char)cells[c].ch : ' ';
  while (n > 0 && out[n - 1] == ' ') n--;
  out[n] = '\0';
}

int main(void)
{
  char buf[PATH_MAX], expected[PATH_MAX], text[81];

  struct terminal *t = start("cd /tmp && printf '\\033]0;smoke title\\007' && exec sleep 5");
  pump(t, 800);
  int len = terminal_cwd(t, buf, sizeof(buf));
  realpath("/tmp", expected);
  CHECK(len > 0 && strcmp(buf, expected) == 0, "terminal_cwd: '%s' (expected '%s')", len > 0 ? buf : "", expected);
  CHECK(strcmp(terminal_title(t), "smoke title") == 0, "terminal_title: '%s'", terminal_title(t));
  terminal_delete(t);

  t = start("printf 'main'; printf '\\033[?1049h\\033[Halt'; printf '\\033[?1049l'; exit 3");
  pump(t, 800);
  row_text(t, 0, text);
  CHECK(strcmp(text, "main") == 0, "alternate screen restored: row 0 = '%s'", text);
  CHECK(terminal_eof_p(t), "EOF after exit");
  CHECK(terminal_collect_exit_status(t) == 3, "exit status 3");
  terminal_delete(t);

  printf("%d failure(s)\n", failures);
  return failures ? 1 : 0;
}
