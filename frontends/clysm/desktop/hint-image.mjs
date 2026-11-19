// hint-image.mjs — build.sh: give the image (lem.wasm) and the modules of the
// heap image (lem.image) the compilation hints of the functions that ran
// when Lem started (train.js):
//   node hint-image.mjs lem.wasm lem.image ran.json
import fs from 'node:fs';
import { withCompilationHints } from './common.mjs';

const [wasmPath, imagePath, ranPath] = process.argv.slice(2);
const ran = JSON.parse(fs.readFileSync(ranPath, 'utf8'));
if (!ran) throw new Error('no functions were noted');
fs.writeFileSync(wasmPath, withCompilationHints(fs.readFileSync(wasmPath), ran.base));

// the heap image: the modules again, with their hints (see host/common.mjs)
const file = fs.readFileSync(imagePath);
const length = file.readUInt32LE(8);
const header = JSON.parse(file.subarray(12, 12 + length).toString());
const modulesOffset = (12 + length + 7) & ~7;
const heap = file.subarray(modulesOffset + header.modulesLength, modulesOffset + header.modulesLength + header.heapLength);
const align8 = (n) => (n + 7) & ~7;
let offset = 0;
// (the modules, or the one module they are linked into, link-image.mjs)
const entries = header.linked ? [header.linked] : header.modules;
const blobs = entries.map((e, i) => {
  const bytes = withCompilationHints(file.subarray(modulesOffset + e.offset, modulesOffset + e.offset + e.length),
                                     ran.modules[i] || []);
  e.offset = offset; e.length = bytes.length;
  offset = align8(offset + bytes.length);
  return bytes;
});
header.modulesLength = offset;
const json = Buffer.from(JSON.stringify(header));
const newModulesOffset = align8(12 + json.length);
const out = Buffer.alloc(newModulesOffset + offset + heap.length);
out.write('CLYSMIMG', 0, 'latin1');
out.writeUInt32LE(json.length, 8);
json.copy(out, 12);
blobs.forEach((b, i) => out.set(b, newModulesOffset + entries[i].offset));
out.set(heap, newModulesOffset + offset);
fs.writeFileSync(imagePath, out);
const count = ran.modules.reduce((n, m) => n + m.length, 0);
console.log(`hints: ${ran.base.length} functions of the image, ${count} of the modules`);
