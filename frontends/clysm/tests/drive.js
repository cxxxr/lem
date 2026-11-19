// frontends/clysm/tests/drive.js — drives Lem in the Electron shell (CLYSM_ELECTRON_TEST):
// it types into the page with sendInputEvent, and checks what Lem has (with
// lisp.eval in the page) and what the page shows.  Prints "ok NAME" or
// "FAIL NAME: ..." for each check, then quits.
// With LEM_WEB_PAGE, the page is the one for a browser (web-shell.js), whose
// files are in memory: the test writes and reads them through Lisp.
const fs = require('node:fs');
const path = require('node:path');

const web = !!process.env.LEM_WEB_PAGE;
const label = web ? 'lem (browser)' : 'lem';
let work = process.env.LEM_TEST_DIR;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

module.exports = (win, app) => {
  const wc = win.webContents;
  win.setContentSize(960, 640);
  const errors = [];
  wc.on('console-message', (event) => {
    // (Lem's log messages are written to the error output)
    if ((event.level === 'error' || event.level === 3) && !/^\[(INFO|WARN)\]/.test(event.message))
      errors.push(event.message);
  });
  const js = (code) => wc.executeJavaScript(code);
  // the printed values of FORM, evaluated in Lem's image
  const lisp = async (form) => (await js(`lisp.eval(${JSON.stringify(form)})`)).replace(/\n$/, '');
  const screen = () => js("document.querySelector('.lem-screen').innerText");
  const key = async (keyCode, modifiers = []) => {
    wc.sendInputEvent({ type: 'keyDown', keyCode, modifiers });
    if (keyCode.length === 1 && modifiers.every((m) => m === 'shift'))
      wc.sendInputEvent({ type: 'char', keyCode, modifiers });
    wc.sendInputEvent({ type: 'keyUp', keyCode, modifiers });
    await sleep(40);
  };
  const type = async (text) => { for (const c of text) await key(c); };
  const command = async (name) => { await key('x', ['alt']); await sleep(100); await type(name); await key('Return'); await sleep(500); };
  let failed = 0;
  const check = (name, ok, detail = '') => {
    if (ok) console.log(`ok ${name}`);
    else { failed++; console.log(`FAIL ${name}${detail ? ': ' + detail : ''}`); }
  };
  const until = async (predicate, ms = 5000) => {
    for (let t = 0; t < ms; t += 100) { if (await predicate()) return true; await sleep(100); }
    return false;
  };
  const lispString = (string) => '"' + string.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"';
  const writeFile = async (file, text) => {
    if (!web) return fs.writeFileSync(file, text);
    await lisp(`(with-open-file (out (ensure-directories-exist ${lispString(file)}) :direction :output :if-exists :supersede) (write-string ${lispString(text)} out))`);
  };
  const readFile = async (file) => {
    if (!web) return fs.readFileSync(file, 'utf8');
    // a string as Lisp prints it: in double quotes, \ before " and \
    const printed = await lisp(`(uiop:read-file-string ${lispString(file)})`);
    return printed.slice(1, -1).replace(/\\(.)/g, '$1');
  };

  wc.once('did-finish-load', async () => {
    try {
      const started = await until(async () => (await js("document.querySelectorAll('.lem-line').length")) > 0, 60000);
      if (web) {
        // the page starts with welcome.lisp; the test types into a new buffer
        check('start', started && await until(async () => (await screen()).includes('welcome.lisp'), 15000));
        work = JSON.parse(await lisp('(namestring (user-homedir-pathname))')) + 'lem-test';
        await key('x', ['control']); await key('b'); await sleep(200);
        await type('test'); await key('Return'); await sleep(200); await key('y'); await sleep(300);
      } else {
        check('start', started && (await screen()).includes('*tmp*'));
      }

      await type('(hello world)'); await key('Return');
      wc.insertText('日本語');       // what an input method commits
      await sleep(300);
      const text = await lisp('(lem:buffer-text (lem:current-buffer))');
      check('typing', text === '"(hello world)\n日本語"', text);
      check('typing shown', (await screen()).includes('(hello world)'));
      // Japanese takes the columns Lem gives it (2 a character), whatever its font
      const wideDrift = await js(`(() => {
        const spans = [...document.querySelectorAll('.lem-line > span.lem-wide')];
        if (!spans.length) return 'no wide text';
        return spans.map((span) => { const r = document.createRange(); r.selectNodeContents(span);
                                     return Math.abs(r.getBoundingClientRect().width - parseFloat(span.style.width)); })
                    .every((d) => d < 1) ? 'ok' : 'drift'; })()`);
      check('wide characters in their columns', wideDrift === 'ok', wideDrift);

      await command('list-buffers');
      check('list-buffers', (await screen()).includes('Buffer') &&
            (await lisp('(lem:get-buffer "*EDITOR ERROR*")')).startsWith('NIL'));
      await key('g', ['control']); await sleep(200);

      const file = path.join(work, 'sample.lisp');
      await writeFile(file, '(defun fact (n)\n  (if (<= n 1) 1 (* n (fact (1- n)))))\n');
      await key('x', ['control']); await key('f', ['control']); await sleep(300);
      await key('a', ['control']); await key('k', ['control']);
      await type(file); await key('Return'); await sleep(800);
      const name = await lisp('(lem:buffer-name (lem:current-buffer))');
      check('find-file', name === '"sample.lisp"', name);
      const mode = () => lisp('(lem:buffer-major-mode (lem:current-buffer))');
      check('lisp mode', await until(async () => (await mode()).includes('LISP-MODE'), 15000), await mode());
      // (the Lisp mode is loaded when the first Lisp file is opened)
      check('highlighting', await until(() => js("[...document.querySelectorAll('.lem-line span')].some((s) => s.innerText === 'defun' && s.style.color)"), 15000));

      // (the first compilation in the page takes a while)
      await key('x', ['alt', 'control']);
      check('eval defun', await until(async () => (await lisp('(cl-user::fact 10)')) === '3628800'),
            await lisp('(cl-user::fact 10)'));

      // C-c M-m: the definition with its macros expanded (sys::macroexpand-all)
      await key('c', ['control']); await key('m', ['alt']);
      const expansion = () => lisp('(let ((b (lem:get-buffer "*lisp-macroexpand*"))) (and b (lem:buffer-text b)))');
      check('macroexpand-all', await until(async () => { const t = await expansion(); return t.startsWith('"(') && !t.includes('(defun'); }),
            await expansion());
      await key(' '); await sleep(300);

      await key('>', ['alt', 'shift']); await type('; saved'); await key('x', ['control']); await key('s', ['control']); await sleep(800);
      check('save', (await readFile(file)).endsWith('; saved'));
      if (web) check('kept by the browser', await js(`localStorage.getItem('lem-clysm:' + ${JSON.stringify(file)}) !== null`));

      // a click moves the cursor: line 2, column 4 (below the line of tabs)
      const [cw, ch] = JSON.parse(await js("JSON.stringify((() => { const s = document.querySelector('.lem-line span'); const l = s.parentElement; return [parseFloat(s.style.width) / s.innerText.length, l.offsetHeight]; })())"));
      wc.sendInputEvent({ type: 'mouseDown', x: Math.round(4.5 * cw), y: Math.round(2.5 * ch), button: 'left', clickCount: 1 });
      wc.sendInputEvent({ type: 'mouseUp', x: Math.round(4.5 * cw), y: Math.round(2.5 * ch), button: 'left', clickCount: 1 });
      await sleep(500);
      const position = await lisp('(list (lem:line-number-at-point (lem:current-point)) (lem:point-charpos (lem:current-point)))');
      check('click', position === '(2 4)', position);

      // a drag selects
      const at = (column, line) => ({ x: Math.round((column + 0.5) * cw), y: Math.round((line + 1.5) * ch) });
      wc.sendInputEvent({ type: 'mouseDown', ...at(2, 0), button: 'left', clickCount: 1 });
      wc.sendInputEvent({ type: 'mouseMove', ...at(6, 0), button: 'left', modifiers: ['leftButtonDown'] });
      wc.sendInputEvent({ type: 'mouseUp', ...at(6, 0), button: 'left', clickCount: 1 });
      await sleep(500);
      const region = await lisp('(and (lem:buffer-mark-p (lem:current-buffer)) (lem:points-to-string (lem:region-beginning (lem:current-buffer)) (lem:region-end (lem:current-buffer))))');
      check('drag', region === '"efun"', region);

      // the wheel scrolls
      const long = path.join(work, 'long.txt');
      await writeFile(long, Array.from({ length: 300 }, (_, i) => `line ${i + 1}`).join('\n'));
      await key('x', ['control']); await key('f', ['control']); await sleep(300);
      await key('a', ['control']); await key('k', ['control']);
      await type(long); await key('Return'); await sleep(800);
      wc.sendInputEvent({ type: 'mouseWheel', ...at(5, 5), deltaX: 0, deltaY: -120, canScroll: true });
      await sleep(800);
      const top = await lisp('(lem:line-number-at-point (lem:window-view-point (lem:current-window)))');
      check('wheel', Number(top) > 1, top);

      await command('start-lisp-repl');
      await type('(list (fact 5) (lisp-implementation-type))'); await key('Return');
      const repl = () => lisp('(lem:buffer-text (lem:get-buffer "*lisp-repl*"))');
      check('repl', await until(async () => (await repl()).includes('(120 \\"Clysm\\")')), await repl());

      // Lem's Lisp mode asks micros, in the same image (frontends/clysm/src/micros.lisp)
      await type('(make-hash'); await key('Tab');
      check('completion', await until(async () => (await repl()).includes('(make-hash-table')), await repl());
      await type(')'); await key('Return');
      check('completed form evaluated', await until(async () => (await repl()).includes('#<HASH-TABLE')));
      await type('(car 5)'); await key('Return');
      const sldb = () => lisp('(lem:buffer-name (lem:current-buffer))');
      check('debugger', await until(async () => (await sldb()).includes('sldb')), await sldb());
      await key('q'); await sleep(500);
      check('debugger left', await until(async () => !(await sldb()).includes('sldb')), await sldb());
      await type('lem:message'); await key('.', ['alt']);
      const definition = () => lisp('(lem:buffer-name (lem:current-buffer))');
      check('find definition', await until(async () => (await definition()) === '"echo.lisp"', 10000), await definition());
      await key(',', ['alt']); await sleep(300);

      // describe (C-c C-d d asks micros for the documentation of a symbol)
      // and the inspector (M-x lisp-inspect, in *lisp-inspector*): asked
      // without waiting (the answer comes in the editor's loop)
      await lisp('(progn (defvar cl-user::*described* nil) (lem-lisp-mode::lisp-eval-async (quote (micros/lsp-api:hover-symbol "mapcar")) (lambda (r) (setf cl-user::*described* r))) t)');
      const described = () => lisp('cl-user::*described*');
      check('describe', await until(async () => /MAPCAR/.test(await described())), await described());
      await lisp('(progn (lem-lisp-mode:lisp-inspect "(list 1 2 3)") t)');
      const inspector = () => lisp('(let ((b (lem:get-buffer "*lisp-inspector*"))) (and b (lem:buffer-text b)))');
      check('inspector', await until(async () => /CONS/.test(await inspector())), await inspector());
      await lisp('(progn (lem:delete-buffer (lem:get-buffer "*lisp-inspector*")) t)'); await sleep(300);

      if (!web) {
        // a world (contrib/world/): an image in a worker, which the Lisp mode
        // connects to; the REPL then evaluates there
        await command('world-start');
        const world = () => lisp('(lem-clysm::current-world)');
        check('world: started', await until(async () => (await world()).includes('world-1 ready'), 30000), await world());
        console.log(`lem: a world started in ${await lisp('(clysm-world:world-seconds (lem-clysm::current-world))')} s`);
        // (the REPL has printed its prompt after the last result)
        const prompt = () => until(async () => (await repl()).endsWith('CL-USER> "'));
        await until(async () => (await repl()).includes('changed connection (clysm'));
        await prompt();
        await type('(defun in-world (n) (* n 2))'); await key('Return');
        await until(async () => (await repl()).includes('IN-WORLD'));
        await prompt();
        await type('(in-world 21)'); await key('Return');
        check('world: the REPL evaluates in it', await until(async () => (await repl()).includes('\n42')), await repl());
        await prompt();
        check('world: not in the editor', (await lisp("(fboundp 'cl-user::in-world)")) === 'NIL');
        // what it writes outside the REPL is in the buffer *worlds*
        await type('(progn (sys::make-thread (lambda () (format t "from a thread~%"))) 7)'); await key('Return');
        const worlds = () => lisp('(let ((b (lem:get-buffer "*worlds*"))) (and b (lem:buffer-text b)))');
        check('world: its output', await until(async () => (await worlds()).includes('[world-1] from a thread')), await worlds());
        await prompt();
        // a computation that does not end leaves the editor as it is; C-c C-c
        // interrupts it
        await type('(loop)'); await key('Return'); await sleep(1000);
        check('world: the editor runs while it loops', (await lisp('(+ 1 2)')) === '3');
        await key('c', ['control']); await key('c', ['control']);
        check('world: interrupt', await until(async () => (await sldb()).includes('sldb')), await sldb());
        await key('q');
        check('world: debugger left', await until(async () => !(await sldb()).includes('sldb')), await sldb());
        // it is killed while it loops, and the Lisp mode is back in the editor
        await prompt();
        await type('(loop)'); await key('Return'); await sleep(500);
        await command('world-kill');
        check('world: killed', (await lisp('(clysm-world:worlds)')) === 'NIL' && (await world()) === 'NIL');
        await until(async () => (await repl()).includes('changed connection (self'));
        await prompt();
        await type('(list (fact 4) (fboundp (quote in-world)))'); await key('Return');
        check('world: the editor\'s image again', await until(async () => (await repl()).includes('(24 NIL)')), await repl());
      }

      // Lem's extensions are loaded when first used (frontends/clysm/src/extensions.lisp)
      const python = path.join(work, 'hello.py');
      await writeFile(python, 'def greet(name):\n    return "Hello, " + name  # a comment\n');
      await key('x', ['control']); await key('f', ['control']); await sleep(300);
      await key('a', ['control']); await key('k', ['control']);
      await type(python); await key('Return');
      const major = () => lisp('(lem:buffer-major-mode (lem:current-buffer))');
      check('mode loaded when a file needs it', await until(async () => (await major()).includes('PYTHON-MODE'), 15000),
            await major());
      check('its highlighting', await until(async () => js("[...document.querySelectorAll('.lem-line span')].some((s) => s.innerText.includes('a comment') && s.style.color)")));

      await command('load-theme');
      await type('nord'); await key('Return');
      const theme = () => lisp('(lem-core::current-theme)');
      check('base16 themes', await until(async () => (await theme()) === '"nord"', 15000), await theme());

      await command('vi-mode');
      check('vi-mode', await until(async () => (await screen()).includes('NORMAL'), 15000));
      await type('ggOimport sys'); await key('Escape'); await sleep(300);
      const first = await lisp('(lem:line-string (lem:buffer-start-point (lem:current-buffer)))');
      check('vi keys', first === '"import sys"', first);
      await command('emacs-mode');
      check('emacs-mode again', await until(async () => !(await screen()).includes('NORMAL')));
      await key('x', ['control']); await key('s', ['control']); await sleep(500);

      if (!web) {
        // a resident application: frontends/clysm/desktop/lem tells it through its
        // socket which files to open (ui/electron/main.js)
        const other = path.join(work, 'from-client.txt');
        fs.writeFileSync(other, 'opened by a client\n');
        const sent = Date.now();
        await new Promise((resolve, reject) => {
          const s = require('node:net').connect(path.join(app.getPath('userData'), 'resident.sock'));
          s.on('error', reject);
          s.end(`open ${other}\nshow\n`, resolve);
        });
        // (the screen shows it: the name of the buffer in its line of tabs)
        const shown = () => js("[...document.querySelectorAll('.lem-line')].some((l) => l.textContent.includes('opened by a client'))");
        let ms = null;
        for (let t = 0; t < 5000; t += 5) {
          if (await shown()) { ms = Date.now() - sent; break; }
          await sleep(5);
        }
        check('resident: a client opens a file', ms !== null, await lisp('(lem:buffer-name (lem:current-buffer))'));
        console.log(`lem: the file of a client on the screen in ${ms} ms`);
      }

      // images: a file opened in the viewer, and one in a buffer's text
      const svg = path.join(work, 'dot.svg');
      await writeFile(svg, '<svg xmlns="http://www.w3.org/2000/svg" width="64" height="48"><circle cx="32" cy="24" r="20" fill="tomato"/></svg>');
      await key('x', ['control']); await key('f', ['control']); await sleep(300);
      await key('a', ['control']); await key('k', ['control']);
      await type(svg); await key('Return');
      const imageShown = (width) => js(`[...document.querySelectorAll('.lem-image img')].some((i) => i.complete && i.naturalWidth === ${width})`);
      check('image viewer', await until(async () => (await lisp('(lem:mode-name (lem:buffer-major-mode (lem:current-buffer)))')) === '"Image Viewer"'
                                             && await imageShown(64)));
      await lisp(`(lem:send-event (lambda () (lem:switch-to-buffer (lem:make-buffer "images")) (lem-clysm:insert-image-file ${lispString(svg)})))`);
      check('image in a buffer', await until(async () => (await lisp('(lem:buffer-name (lem:current-buffer))')) === '"images"' && await imageShown(64)));

      // text of several sizes and an image in one buffer, on one baseline
      await lisp('(lem:send-event (lambda () (lem-clysm:rich-text-sample)))');
      check('text of several sizes', await until(() => js(`(() => {
        const spans = [...document.querySelectorAll('.lem-line > span')];
        const title = spans.find((s) => s.textContent.includes('em on Clysm') && s.style.fontSize === '2.5em');
        const img = document.querySelector('.lem-image img');
        const after = spans.find((s) => s.textContent.includes('with text on its baseline'));
        return !!(title && img && img.complete && after
                  && after.parentElement === img.parentElement.parentElement);
      })()`)));

      // the larger text does not move where the cursor is (Lem cuts it there)
      const titleEnd = () => js(`(() => { const l = [...document.querySelectorAll('.lem-line')]
          .find((l) => l.textContent.includes('on Clysm') && [...l.children].some((s) => s.style.fontSize === '2.5em'));
        const last = l && l.lastElementChild;
        return last ? parseFloat(last.style.left) + parseFloat(last.style.width) : null; })()`);
      const ends = [];
      for (let i = 0; i < 4; i++) { ends.push(await titleEnd()); await key('f', ['control']); await sleep(150); }
      check('larger text under the cursor', ends[0] !== null && ends.every((e) => Math.abs(e - ends[0]) < 0.5), ends.join(' '));

      // Markdown as it reads: a larger heading, the marks hidden, an image
      const md = path.join(work, 'notes.md');
      await writeFile(md, '# Title\n\nSome **bold** text.\n\n![a dot](dot.svg)\n\n- [ ] a task\n');
      await lisp(`(lem:send-event (lambda () (lem:find-file ${lispString(md)})))`);
      await key('n', ['control']); await sleep(300);   // (a command: the buffer is decorated after it)
      const rendered = () => js(`(() => {
        const spans = [...document.querySelectorAll('.lem-windows .lem-line > span')];
        const title = spans.find((s) => s.textContent.includes('Title'));
        const lines = [...document.querySelectorAll('.lem-windows .lem-line')].map((l) => l.textContent);
        return !!(title && title.style.fontSize === '2em' && !title.textContent.includes('#')
                  && lines.some((l) => l.includes('Some bold text.'))
                  && lines.some((l) => l.includes('☐ a task'))
                  && [...document.querySelectorAll('.lem-windows .lem-image img')].some((i) => i.complete && i.naturalWidth === 64));
      })()`);
      check('markdown rendered', await until(rendered, 15000),
            await js("[...document.querySelectorAll('.lem-windows .lem-line')].map((l) => l.textContent).join(' | ')"));
      // the line of the cursor shows its marks
      await key('n', ['control']); await sleep(300);
      check('markdown marks on the line of the cursor',
            await until(() => js("[...document.querySelectorAll('.lem-windows .lem-line')].some((l) => l.textContent.includes('Some **bold** text.'))")));
      await lisp('(lem:send-event (lambda () (lem:move-to-line (lem:current-point) 7) (lem-clysm:markdown-toggle-task)))');
      check('markdown task toggled', await until(async () => (await lisp('(lem:buffer-text (lem:current-buffer))')).includes('- [x] a task')));
      await key('x', ['control']); await key('s', ['control']); await sleep(300);   // (saved: C-x C-c does not ask)
      // Claude Code, with tests/fake-claude.mjs (LEM_CLAUDE_COMMAND): in the desktop application
      if (!web) {
        const project = path.join(work, 'project');
        fs.mkdirSync(path.join(project, '.git'), { recursive: true });   // (the root of a project)
        fs.writeFileSync(path.join(project, 'notes.txt'), 'notes\n');
        await lisp(`(lem:send-event (lambda () (lem:find-file ${lispString(path.join(project, 'notes.txt'))}) (lem-clysm:claude-code)))`);
        await sleep(1000);
        await type('hello claude'); await key('Return');
        const chat = () => lisp('(lem:buffer-text (lem-clysm::session-chat (first lem-clysm::*claude-sessions*)))');
        check('claude asks', await until(async () => (await chat()).includes('Allow?'), 10000), await chat());
        await key('y');
        check('claude answers', await until(async () => { const c = await chat(); return c.includes('Done: **fake.txt** says hi.') && c.includes('$0.0012'); }, 10000), await chat());
        check('claude wrote the file', fs.existsSync(path.join(project, 'fake.txt')));
        await lisp('(lem:send-event (lambda () (lem-core/commands/window:delete-other-windows)))');
      }

      // the editor seen from outside (M-x remote-start, tools/lem-remote): in the desktop application
      if (!web) {
        await lisp('(lem:send-event (lambda () (lem-clysm:remote-start)))');
        const remoteFile = path.join(process.env.LEM_HOME, 'remote.json');
        check('remote: started', await until(() => fs.existsSync(remoteFile)));
        const { url, token } = JSON.parse(fs.readFileSync(remoteFile, 'utf8'));
        const ask = async (method, route, body, headers = {}) => {
          const response = await fetch(url + route, { method, body, headers: { authorization: 'Bearer ' + token, ...headers } });
          return [response.status, await response.text()];
        };
        const [, state] = await ask('GET', '/state');
        check('remote: state', state.includes('buffers:') && state.includes('windows:'), state);
        const [, value] = await ask('POST', '/eval', '(+ 1 2)');
        check('remote: eval', value.trim() === '3', value);
        const [, screen] = await ask('GET', '/screen');
        check('remote: screen', screen.includes('== window'), screen);
        const [, line] = await ask('POST', '/eval', `(lem-clysm::segments-text '((0 6 "日本語" "" "lem-wide") (6 3 "abc" "") (9 4 "Big" "" nil nil 4/3)) 20)`);
        check('remote: wide and larger text on the screen', line.trim() === '"日本語abcBig"', line);
        check('remote: no token, no answer', (await fetch(url + '/state')).status === 401);
        await lisp('(lem:send-event (lambda () (lem-clysm:remote-stop)))');
        check('remote: stopped', await until(() => !fs.existsSync(remoteFile)));
      }

      // folds in source code: C-c Tab on a form
      await lisp(`(lem:send-event (lambda () (lem:find-file ${lispString(file)}) (lem:buffer-start (lem:current-point))))`);
      await sleep(500);
      await key('c', ['control']); await key('Tab'); await sleep(300);
      const codeText = () => js("[...document.querySelectorAll('.lem-windows .lem-window .lem-line')].map((l) => l.textContent).join(' | ')");
      check('fold a form', await until(async () => { const t = await codeText(); return t.includes('(defun fact (n)...') && !t.includes('(if (<= n 1)'); }), await codeText());
      await key('c', ['control']); await key('Tab'); await sleep(300);
      check('unfold it', await until(async () => (await codeText()).includes('(if (<= n 1)')), await codeText());
      await lisp(`(lem:send-event (lambda () (lem:find-file ${lispString(md)})))`); await sleep(500);
      // folds: Tab on a heading folds its section; Shift-Tab: the outline, the headings, all
      const shownText = () => js("[...document.querySelectorAll('.lem-windows .lem-window .lem-line')].map((l) => l.textContent).join(' | ')");
      await lisp('(lem:send-event (lambda () (lem:buffer-start (lem:current-point))))'); await sleep(200);
      await key('Tab'); await sleep(300);
      check('fold a section', await until(async () => { const t = await shownText(); return t.includes('⋯') && !t.includes('Some bold text.'); }), await shownText());
      await key('Tab'); await sleep(300);
      check('unfold it', await until(async () => (await shownText()).includes('Some bold text.')), await shownText());
      await key('Tab', ['shift']); await sleep(300);
      check('outline', await until(async () => { const t = await shownText(); return t.includes('Title') && !t.includes('a task'); }), await shownText());
      await key('Tab', ['shift']); await key('Tab', ['shift']); await sleep(300);
      check('all shown again', await until(async () => (await shownText()).includes('a task')), await shownText());

      const width = await lisp('(lem:display-width)');
      win.setContentSize(700, 400);
      check('resize', await until(async () => (await lisp('(lem:display-width)')) !== width));

      check('no errors on the console', errors.length === 0, errors.join(' | '));
      const finish = () => { console.log(failed ? `${label}: ${failed} failed` : `${label}: ok`); app.exit(failed ? 1 : 0); };
      win.on('closed', finish);
      await key('x', ['control']); await key('c', ['control']);
      // (a browser may not let the page close: then it says that Lem has exited)
      const exited = await until(async () => win.isDestroyed() || (await js("document.body.innerText")).includes('Lem has exited'), 3000);
      check('exit', exited, 'C-x C-c did not close the window');
      finish();
    } catch (e) {
      console.log('FAIL', e && e.stack || e);
    }
    app.exit(1);
  });
};
