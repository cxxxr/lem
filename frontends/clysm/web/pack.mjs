// frontends/clysm/web/pack.mjs — pack the files that loading Lem reads into
// one gzip-compressed file for the page (the format is in host/web/vfs.mjs):
//   node pack.mjs TRACE.json OUT.pack.gz [SOURCE-DIRECTORY...]
// TRACE.json is what host/run.mjs wrote with CLYSM_TRACE_FILES: the files
// read are packed with their contents, the other paths seen with their
// names and times only (ASDF compares the times of sources and fasls).
// Under the SOURCE-DIRECTORYs, the Lisp sources are packed with their
// contents too, for M-. of the Lisp mode: those seen, those named in
// LIST:FILE (a file of paths, one per line: the sources of what the page
// loads, which it does not look at), and every one under a directory given
// as TREE:DIRECTORY (Clysm's runtime, whose definitions the image knows).
// FILE:PATH packs the file PATH with its contents (the heap image).
import fs from 'node:fs';
import zlib from 'node:zlib';
import path from 'node:path';

const [traceFile, out, ...sourceArgs] = process.argv.slice(2);
const { read, seen } = JSON.parse(fs.readFileSync(traceFile, 'utf8'));
const sourceDirs = sourceArgs.filter((d) => !/^(TREE|LIST|FILE):/.test(d)).map((d) => path.resolve(d) + '/');
const extraFiles = sourceArgs.filter((d) => d.startsWith('FILE:')).map((d) => path.resolve(d.slice(5)));
const listed = sourceArgs.filter((d) => d.startsWith('LIST:'))
  .flatMap((d) => fs.readFileSync(d.slice(5), 'utf8').split('\n').filter(Boolean))
  .filter((p) => sourceDirs.some((d) => p.startsWith(d)));
const trees = sourceArgs.filter((d) => d.startsWith('TREE:')).map((d) => path.resolve(d.slice(5)));
const lispUnder = (dir) => fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) =>
  e.isDirectory() ? lispUnder(path.join(dir, e.name)) : e.name.endsWith('.lisp') ? [path.join(dir, e.name)] : []);
const treeFiles = trees.flatMap(lispUnder);
const readSet = new Set([...read, ...treeFiles, ...listed, ...extraFiles,
                         ...seen.filter((p) => p.endsWith('.lisp') && sourceDirs.some((d) => p.startsWith(d)))]);
const header = [];
const contents = [];
for (const path of new Set([...read, ...seen, ...treeFiles, ...listed, ...extraFiles])) {
  if (!path.startsWith('/')) continue;
  let stat;
  try { stat = fs.statSync(path); } catch { continue; }
  if (stat.isDirectory()) { header.push({ path: path.replace(/\/+$/, '') || '/', directory: true }); continue; }
  const content = readSet.has(path);
  header.push({ path, size: content ? stat.size : 0, mtime: stat.mtimeMs, content });
  if (content) contents.push(fs.readFileSync(path));
}
const headerOctets = Buffer.from(JSON.stringify(header));
const length = Buffer.alloc(4);
length.writeUInt32LE(headerOctets.length);
const pack = Buffer.concat([Buffer.from('CLYSMPAK'), length, headerOctets, ...contents]);
fs.writeFileSync(out, zlib.gzipSync(pack, { level: 9 }));
console.log(`${out}: ${header.length} files, ${pack.length} octets, ${fs.statSync(out).size} compressed`);
