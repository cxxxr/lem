// link-image.mjs — build.sh: the modules of the heap image lem.image linked
// into one module (host/link.mjs), which the page compiles and instantiates
// at once:
//   node link-image.mjs lem.image
import fs from 'node:fs';
import { linkModules } from './link.mjs';

const [imagePath] = process.argv.slice(2);
const file = fs.readFileSync(imagePath);
const length = file.readUInt32LE(8);
const header = JSON.parse(file.subarray(12, 12 + length).toString());
if (header.linked) process.exit(0);
const modulesOffset = (12 + length + 7) & ~7;
const heap = file.subarray(modulesOffset + header.modulesLength, modulesOffset + header.modulesLength + header.heapLength);
const linked = linkModules(header.modules.map((e) => ({
  bytes: file.subarray(modulesOffset + e.offset, modulesOffset + e.offset + e.length), codeBase: e.codeBase })));
// (the module is compiled now to tell an error here rather than in the page)
new WebAssembly.Module(linked);
const align8 = (n) => (n + 7) & ~7;
header.linked = { offset: 0, length: linked.length };
header.modules = header.modules.map((e) => ({ codeBase: e.codeBase }));
header.modulesLength = align8(linked.length);
const json = Buffer.from(JSON.stringify(header));
const newModulesOffset = align8(12 + json.length);
const out = Buffer.alloc(newModulesOffset + header.modulesLength + heap.length);
out.write('CLYSMIMG', 0, 'latin1');
out.writeUInt32LE(json.length, 8);
json.copy(out, 12);
out.set(linked, newModulesOffset);
out.set(heap, newModulesOffset + header.modulesLength);
fs.writeFileSync(imagePath, out);
console.log(`linked ${header.modules.length} modules into one (${(linked.length / 1e6).toFixed(1)} MB)`);
