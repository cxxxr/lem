#!/usr/bin/env node
// frontends/clysm/tests/fake-claude.mjs — a claude command for make test-clysm
// (LEM_CLAUDE_COMMAND): it speaks claude -p's JSON lines (stream-json, the
// control requests) without a model.  To each message it answers with a
// streamed text, then asks to write fake.txt (can_use_tool), writes it if
// allowed, and says it is done.
import readline from 'node:readline';
import fs from 'node:fs';
import path from 'node:path';

const out = (o) => process.stdout.write(JSON.stringify(o) + '\n');
const session_id = 'fake-session';
let n = 0;
let waiting = null;
out({ type: 'system', subtype: 'init', session_id, model: 'fake-model', cwd: process.cwd(), tools: ['Write'] });

const text = (id, s) => {
  out({ type: 'stream_event', event: { type: 'message_start', message: { id } } });
  out({ type: 'stream_event', event: { type: 'content_block_start', index: 0, content_block: { type: 'text', text: '' } } });
  for (const piece of s.match(/.{1,6}/gs)) out({ type: 'stream_event', event: { type: 'content_block_delta', index: 0, delta: { type: 'text_delta', text: piece } } });
  out({ type: 'stream_event', event: { type: 'content_block_stop', index: 0 } });
  out({ type: 'assistant', message: { id, role: 'assistant', content: [{ type: 'text', text: s }] }, session_id });
};

readline.createInterface({ input: process.stdin }).on('line', (line) => {
  const m = JSON.parse(line);
  if (m.type === 'control_request') {
    const { subtype, mode } = m.request;
    if (subtype === 'set_permission_mode' && mode === 'bypassPermissions'
        && !process.argv.includes('--allow-dangerously-skip-permissions')) {
      out({ type: 'control_response', response: { subtype: 'error', request_id: m.request_id,
            error: 'Cannot set permission mode to bypassPermissions because the session was not launched with --dangerously-skip-permissions',
            error_code: 'bypass_not_launched' } });
    } else {
      out({ type: 'control_response', response: { subtype: 'success', request_id: m.request_id,
            response: subtype === 'set_permission_mode' ? { mode } : {} } });
    }
  } else if (m.type === 'control_response' && waiting) {
    const allow = m.response.response.behavior === 'allow';
    const { file, id } = waiting;
    waiting = null;
    if (allow) fs.writeFileSync(file, 'hi\n');
    out({ type: 'user', message: { role: 'user', content: [{ type: 'tool_result', tool_use_id: id, content: allow ? `File created successfully at: ${file}\n\`\`\`lisp\n(a code block cut off` : 'The user does not allow it.', is_error: !allow }] },
          tool_use_result: allow ? { type: 'create', filePath: file, content: 'hi\n', structuredPatch: [] } : undefined, session_id });
    text('m' + (++n), allow ? 'Done: **fake.txt** says hi.' : 'Not written.');
    out({ type: 'result', subtype: 'success', duration_ms: 1200, total_cost_usd: 0.0012, session_id });
  } else if (m.type === 'user') {
    const said = m.message.content[0].text;
    text('m' + (++n), `You said *${said}*.\n\nI will write a file:`);
    const file = path.join(process.cwd(), 'fake.txt');
    const id = 'toolu_' + (++n);
    waiting = { file, id };
    out({ type: 'assistant', message: { id: 'm' + n, role: 'assistant', content: [{ type: 'tool_use', id, name: 'Write', input: { file_path: file, content: 'hi\n' } }] }, session_id });
    out({ type: 'control_request', request_id: 'req-' + n, request: { subtype: 'can_use_tool', tool_name: 'Write', input: { file_path: file, content: 'hi\n' }, permission_suggestions: [{ type: 'setMode', mode: 'acceptEdits', destination: 'session' }], tool_use_id: id } });
  }
});
