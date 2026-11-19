// frontends/clysm/web/make-html.mjs — make the page of Lem in one file:
//   node make-html.mjs CLYSM-ROOT IMAGE.wasm FILES.pack.gz BOOT.lisp HOME COUNT > lem.html
// COUNT is the number of files that loading Lem loads (for the page's progress bar).
// The host modules are put into the page (a page opened from a file cannot
// import modules), and the image and the files as base64 text.
import fs from 'node:fs';
import zlib from 'node:zlib';
import path from 'node:path';

const [root, image, files, boot, home, count] = process.argv.slice(2);
const here = path.dirname(new URL(import.meta.url).pathname);
const module = (file) => fs.readFileSync(path.join(root, file), 'utf8')
  .replace(/^import [^;]*;/gm, '')
  .replace(/^export /gm, '');
const host = ['host/jsffi.mjs', 'host/common.mjs', 'host/web/clysm.mjs', 'host/web/vfs.mjs'].map(module).join('\n');
const values = {
  CSS: fs.readFileSync(path.join(here, '..', 'lem.css'), 'utf8'),
  HOST: host,
  IMAGE: zlib.gzipSync(fs.readFileSync(image), { level: 9 }).toString('base64'),
  FILES: fs.readFileSync(files).toString('base64'),
  HOME: JSON.stringify(home),
  ROOT: JSON.stringify(root.replace(/\/?$/, '/')),
  BOOT: JSON.stringify(boot),
  COUNT: String(parseInt(count, 10) || 1),
};
// (a function as the replacement: the values contain "$")
process.stdout.write(fs.readFileSync(path.join(here, 'index.html'), 'utf8')
  .replace(/@([A-Z]+)@/g, (all, name) => (name in values ? values[name] : all)));
