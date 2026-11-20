;;;; claude.lisp — Claude Code in Lem, in the desktop application
;;;;
;;;; M-x claude starts Claude Code (the claude command) for the project
;;;; of the buffer, in the background, and talks with it in two buffers: the
;;;; conversation (*claude: PROJECT*), shown as Markdown as it reads
;;;; (markdown.lisp), and the input, a box attached to the bottom of the
;;;; conversation's window (Lem's attached windows: it goes with the
;;;; conversation when windows are deleted, split or switched), where Return
;;;; sends.
;;;;
;;;; claude runs as the Agent SDK runs it: claude -p with JSON lines in and
;;;; out (--input-format/--output-format stream-json), started by Node.js's
;;;; child_process in the page of the desktop application.  The answer is
;;;; written as it comes (--include-partial-messages); before using a tool
;;;; that needs it, claude asks (a control_request can_use_tool), and the
;;;; conversation shows what it would do (the diff of an edit, the command)
;;;; and waits for y (yes), a (yes, and the same from now on) or n (no),
;;;; asked in the input box (what was typed there is put back after).
;;;; What each tool did is under a line that Tab unfolds.  Files that claude
;;;; changed are read again in the buffers that have them (unless they were
;;;; changed there).  Shift-Tab in the input cycles the permission mode
;;;; (default, acceptEdits, plan), C-c C-c interrupts.

(in-package :lem-clysm)

(defvar *claude-command* "claude"
  "The command of Claude Code.  The environment variable LEM_CLAUDE_COMMAND,
if set, replaces it.  Without a directory, it is looked for in PATH, then in
~/.local/bin.")

(defvar *claude-arguments* '()
  "More arguments of claude (\"--model\" \"opus\"...).")

(defparameter *claude-permission-modes* '("default" "acceptEdits" "plan")
  "The permission modes that Shift-Tab cycles through (and bypassPermissions
after them, with *CLAUDE-ALLOW-BYPASS-PERMISSIONS*).")

(defvar *claude-allow-bypass-permissions* nil
  "Whether Claude may run every tool without asking (bypassPermissions): when
true, claude is started with --allow-dangerously-skip-permissions and
Shift-Tab goes to bypassPermissions after plan.  Claude then edits, deletes
and runs commands without a question: for sandboxes, or what can be lost.")

(defvar *claude-permission-mode* nil
  "The permission mode a session starts in (\"acceptEdits\", \"plan\",
\"bypassPermissions\" with *CLAUDE-ALLOW-BYPASS-PERMISSIONS*...), or NIL for
claude's default.")

;;; ---- the process (Node.js, in the desktop application)

(sys::%defvar-host *claude-spawn*
  (js:eval "(command, cwd, args, onMessage, onExit) => {
     const { spawn } = require('node:child_process');
     const readline = require('node:readline');
     const fs = require('node:fs'), path = require('node:path'), os = require('node:os');
     let file = process.env.LEM_CLAUDE_COMMAND || command;
     if (!file.includes('/')) {
       const executable = (f) => { try { fs.accessSync(f, fs.constants.X_OK); return true; } catch { return false; } };
       const dirs = (process.env.PATH || '').split(path.delimiter).concat([path.join(os.homedir(), '.local', 'bin')]);
       file = dirs.map((d) => path.join(d, file)).find(executable) || file;
     }
     const child = spawn(file, args, { cwd, stdio: ['pipe', 'pipe', 'pipe'] });
     let errors = '';
     child.stderr.on('data', (d) => { errors = (errors + d).slice(-4000); });
     readline.createInterface({ input: child.stdout }).on('line', (line) => {
       let message;
       try { message = JSON.parse(line); } catch { return; }
       onMessage(message);
     });
     let ended = false;
     const end = (code, why) => { if (!ended) { ended = true; onExit(code, why); } };
     child.on('error', (e) => end(-1, String(e.message) + ' (' + file + ')'));
     child.on('exit', (code) => end(code === null ? -1 : code, errors));
     let ids = 0;
     return {
       send: (o) => { if (!ended) child.stdin.write(JSON.stringify(o) + '\\n'); },
       control: (subtype, key, value) => {
         const request = { subtype };
         if (key) request[key] = value;
         if (!ended) child.stdin.write(JSON.stringify({ type: 'control_request', request_id: 'lem-' + (++ids), request }) + '\\n');
       },
       kill: () => { child.stdin.end(); child.kill(); },
     };
   }"))

(sys::%defvar-host *claude-send-user*
  (js:eval "(handle, text) => handle.send({ type: 'user', message: { role: 'user', content: [{ type: 'text', text }] } })"))

(sys::%defvar-host *claude-answer*
  (js:eval "(handle, request, allow, message, always) => {
     const response = allow
       ? { behavior: 'allow', updatedInput: request.request.input,
           ...(always ? { updatedPermissions: request.request.permission_suggestions || [] } : {}) }
       : { behavior: 'deny', message: message || 'The user does not allow it.' };
     handle.send({ type: 'control_response',
                   response: { subtype: 'success', request_id: request.request_id, response } });
   }"))

(sys::%defvar-host *json-text*
  (js:eval "(value, pretty) => { try { return JSON.stringify(value, null, pretty ? 2 : undefined) || ''; } catch { return ''; } }"))

(sys::%defvar-host *node-available*
  (js:eval "() => typeof require === 'function' && typeof process === 'object'"))

;;; JSON from claude is JavaScript values: objects and arrays stay
;;; JavaScript values, strings, numbers and booleans are Lisp ones (null and
;;; undefined are NIL).

(defun jget (value &rest keys)
  "The value of VALUE at KEYS (names and indexes), or NIL."
  (dolist (key keys value)
    (setf value (and (js:js-value-p value) (js:get value key)))))

(defun jstring (value &rest keys)
  (let ((v (apply #'jget value keys)))
    (and (stringp v) v)))

(defun jlist (value &rest keys)
  "The elements of the array at KEYS of VALUE, as a list."
  (let ((array (apply #'jget value keys)))
    (when (js:js-value-p array)
      (loop for i below (or (js:get array "length") 0)
            collect (js:get array i)))))

;;; ---- a session: a claude process, its conversation and its input

(defstruct (claude-session (:conc-name session-))
  handle directory chat input
  (state :starting)                     ; :starting :idle :responding :exited
  session-id model (mode "default") cost
  (bypass-allowed nil)                  ; started with --allow-dangerously-skip-permissions
  (pending '())                         ; permission requests waiting for an answer
  (draft nil)                           ; (text . position) of the input while it asks
  (asked-at 0)                          ; when the question was put in the input
  (streamed '())                        ; ids of the messages whose text was streamed
  (in-text nil)                         ; a text is being streamed
  (tools (make-hash-table :test 'equal)) ; tool_use_id -> name
  (redraw nil))                         ; a redraw is scheduled

(defvar *claude-sessions* '() "The sessions, the latest first.")

(defun buffer-session (buffer)
  (find-if (lambda (session)
             (or (eq buffer (session-chat session)) (eq buffer (session-input session))))
           *claude-sessions*))

(defun current-session ()
  "The session of the current buffer, or the latest one."
  (or (buffer-session (lem:current-buffer))
      (first *claude-sessions*)
      (lem:editor-error "No Claude Code session: M-x claude")))

;;; ---- writing the conversation

(defun claude-attribute (name)
  (md-attribute name))

(defun chat-end (session)
  (lem:buffer-end-point (session-chat session)))

(defun following-windows (session)
  "The windows of the conversation whose point is at its end: they follow
what is written."
  (let ((end (chat-end session)))
    (remove-if-not (lambda (window) (lem:point= (lem:window-point window) end))
                   (lem:get-buffer-windows (session-chat session)))))

(defun write-chat (session string &optional attribute)
  "Write STRING at the end of the conversation, with ATTRIBUTE (a name of
md-attribute, an attribute or NIL)."
  (let ((windows (following-windows session))
        (lem:*inhibit-read-only* t))
    (lem:insert-string (chat-end session) string
                       :attribute (if (keywordp attribute) (claude-attribute attribute) attribute))
    (dolist (window windows)
      (lem:move-point (lem:window-point window) (chat-end session)))
    (schedule-redraw session)))

(defun fresh-chat-line (session)
  (let ((end (chat-end session)))
    (unless (lem:start-line-p end)
      (write-chat session (string #\Newline)))))

(defun blank-chat-line (session)
  "End the conversation with an empty line (before something new)."
  (fresh-chat-line session)
  (lem:with-point ((p (chat-end session)))
    (when (and (lem:line-offset p -1) (plusp (length (lem:line-string p))))
      (write-chat session (string #\Newline)))))

(defun schedule-redraw (session)
  "Draw the screen again soon (the answer comes in many small pieces)."
  (unless (session-redraw session)
    (setf (session-redraw session) t)
    (lem:start-timer (lem:make-timer (lambda ()
                                       (setf (session-redraw session) nil)
                                       (refresh-markdown-buffers)
                                       (lem:redraw-display))
                                     :name "claude-redraw")
                     40)))

(defun fold-lines-after (session header-point)
  "Fold what was written after the line of HEADER-POINT (Tab unfolds it)."
  (lem:with-point ((start header-point)
                   (end (chat-end session)))
    (lem:line-start start)
    (unless (lem:same-line-p start end)
      (lem:place-region-placeholder-overlay start end :placeholder " ⋯"))))

;;; ---- tools

(defun short (string &optional (length 72))
  (let ((line (subseq string 0 (or (position #\Newline string) (length string)))))
    (if (< length (length line))
        (concatenate 'string (subseq line 0 (- length 1)) "…")
        line)))

(defun relative-path (session path)
  (let ((directory (namestring (session-directory session))))
    (if (and path (eql 0 (search directory path)))
        (subseq path (length directory))
        path)))

(defun tool-summary (session name input)
  "What the tool NAME does with INPUT, in a few words."
  (let ((path (or (jstring input "file_path") (jstring input "notebook_path") (jstring input "path"))))
    (short (or (cond ((jstring input "command"))
                     (path (relative-path session path))
                     ((jstring input "pattern"))
                     ((jstring input "url"))
                     ((jstring input "query"))
                     ((jstring input "description"))
                     ((jstring input "prompt")))
               (js:funcall *json-text* input nil)))))

(defun write-fenced (session language text &key (max-lines 40))
  "Write TEXT in a code block of LANGUAGE (at most MAX-LINES lines), between
fences longer than those of TEXT: a ``` of TEXT (a Markdown file read) does
not end the block."
  (let* ((lines (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline)))
         (shown (subseq lines 0 (min max-lines (length lines))))
         (fence (make-string (1+ (reduce #'max shown
                                         :key (lambda (line)
                                                (let ((fence (fence-p line)))
                                                  (if (and fence (char= (char fence 0) #\`))
                                                      (length fence)
                                                      2)))
                                         :initial-value 2))
                             :initial-element #\`)))
    (fresh-chat-line session)
    (write-chat session (format nil "~A~A~%~{~A~%~}~A~%" fence language shown fence))
    (when (< max-lines (length lines))
      (write-chat session (format nil "… ~D more lines~%" (- (length lines) max-lines)) :missing))))

(defun diff-text (old new)
  "A diff of OLD and NEW (an edit's strings): what is removed, then what is added."
  (with-output-to-string (out)
    (dolist (line (uiop:split-string old :separator '(#\Newline)))
      (format out "-~A~%" line))
    (dolist (line (uiop:split-string new :separator '(#\Newline)))
      (format out "+~A~%" line))))

(defun write-tool-preview (session name input)
  "What the tool would change: the diff of an edit, the new file, the command."
  (cond ((member name '("Edit") :test #'string=)
         (write-fenced session "diff" (diff-text (or (jstring input "old_string") "")
                                                 (or (jstring input "new_string") ""))))
        ((string= name "MultiEdit")
         (write-fenced session "diff"
                       (apply #'concatenate 'string
                              (mapcar (lambda (edit)
                                        (diff-text (or (jstring edit "old_string") "")
                                                   (or (jstring edit "new_string") "")))
                                      (jlist input "edits")))))
        ((string= name "Write")
         (write-fenced session "diff"
                       (format nil "~{+~A~%~}"
                               (uiop:split-string (string-right-trim '(#\Newline) (or (jstring input "content") ""))
                                                  :separator '(#\Newline)))))
        ((string= name "Bash")
         (write-fenced session "sh" (or (jstring input "command") "")))))

(defun write-tool-use (session content)
  (let ((name (or (jstring content "name") "?"))
        (input (jget content "input")))
    (setf (gethash (jstring content "id") (session-tools session)) name)
    (fresh-chat-line session)
    (write-chat session "⏺ " :bullet)
    (write-chat session name :bold)
    (write-chat session (format nil "  ~A~%" (tool-summary session name input)) :mark)))

(defun result-text (content)
  "The text of a tool_result's content (a string, or text blocks)."
  (let ((value (jget content "content")))
    (cond ((stringp value) value)
          (t (format nil "~{~A~^~%~}"
                     (loop for block in (jlist content "content")
                           for text = (jstring block "text")
                           when text collect text))))))

(defun write-tool-result (session content tool-use-result)
  "The result of a tool: a line, and what it said or changed folded under it."
  (let* ((error (jget content "is_error"))
         (text (result-text content))
         (lines (count #\Newline (string-right-trim '(#\Newline) text)))
         (patch (jlist tool-use-result "structuredPatch")))
    (fresh-chat-line session)
    (lem:with-point ((header (chat-end session) :right-inserting))
      (lem:line-start header)
      (write-chat session "  ⎿ " :mark)
      (write-chat session (format nil "~A~%" (short (if (plusp (length text)) text "(nothing)")))
                  (if error :removed :missing))
      (cond (patch
             (write-fenced session "diff"
                           (format nil "~{~A~%~}"
                                   (loop for hunk in patch
                                         collect (format nil "@@ -~A +~A @@" (jget hunk "oldStart") (jget hunk "newStart"))
                                         append (loop for line in (jlist hunk "lines")
                                                      when (stringp line) collect line)))
                           :max-lines 200)
             (fold-lines-after session header))
            ((plusp lines)
             (write-fenced session "" text :max-lines 200)
             (fold-lines-after session header))))
    (let ((file (jstring tool-use-result "filePath")))
      (when (and file (not error))
        (reload-file file)))))

(defun reload-file (file)
  "Read FILE again in the buffer that has it, unless it was changed there."
  (let ((buffer (lem:get-file-buffer file)))
    (when (and buffer (not (lem:buffer-modified-p buffer)))
      (lem:with-current-buffer buffer
        (lem-core/commands/file:revert-buffer t)))))

;;; ---- permissions

(defvar *claude-answer-delay* 0.3
  "Seconds after a question is put in the input during which y, a and n do
not answer it: keys typed for the input just before it came are not taken
as an answer.")

(defun permission-question (session request)
  "The question of REQUEST, for the input: (string . attribute) pieces."
  (let ((name (or (jstring request "request" "tool_name") "?")))
    (list (cons "Allow " :bold)
          (cons name :bold)
          (cons (format nil "  ~A~%" (tool-summary session name (jget request "request" "input")))
                :mark)
          (cons (format nil "y yes · a yes, and don't ask again~@[ (~A)~] · n no"
                        (permission-suggestion request))
                :link))))

(defun ask-in-input (session request)
  "Put the question of REQUEST in the input (what was typed is kept, to be
put back), read-only until it is answered."
  (let ((input (session-input session))
        (lem:*inhibit-read-only* t))
    (unless (session-draft session)
      (setf (session-draft session)
            (cons (lem:buffer-text input)
                  (lem:position-at-point (lem:buffer-point input)))))
    (lem:erase-buffer input)
    (lem:with-point ((p (lem:buffer-end-point input) :left-inserting))
      (loop :for (string . attribute) :in (permission-question session request)
            :do (lem:insert-string p string :attribute (claude-attribute attribute))))
    (lem:buffer-start (lem:buffer-point input))
    (setf (lem:buffer-read-only-p input) t
          (session-asked-at session) (get-internal-real-time))))

(defun restore-input (session)
  "Put back in the input what was typed before a question."
  (let ((draft (session-draft session))
        (input (session-input session))
        (lem:*inhibit-read-only* t))
    (when draft
      (setf (session-draft session) nil
            (lem:buffer-read-only-p input) nil)
      (lem:erase-buffer input)
      (lem:insert-string (lem:buffer-end-point input) (car draft))
      (lem:move-to-position (lem:buffer-point input)
                            (min (cdr draft) (1+ (length (car draft))))))))

(defun write-permission-request (session request)
  (let* ((name (or (jstring request "request" "tool_name") "?"))
         (input (jget request "request" "input")))
    ;; (the line of the tool is written just before: what it would change, then the question)
    (fresh-chat-line session)
    (write-tool-preview session name input)
    (write-chat session "  Allow?  " :bold)
    (write-chat session (format nil "y yes · a yes, and don't ask again~@[ (~A)~] · n no~%"
                                (permission-suggestion request))
                :link)
    (ask-in-input session request)))

(defun permission-suggestion (request)
  "What \"don't ask again\" would allow, in a few words."
  (let ((suggestion (first (jlist request "request" "permission_suggestions"))))
    (cond ((null suggestion) nil)
          ((jstring suggestion "mode") (format nil "mode ~A" (jstring suggestion "mode")))
          (t (short (js:funcall *json-text* suggestion nil) 40)))))

(defun answer-permission (session allow &key always message)
  (let ((request (first (session-pending session))))
    (unless request (lem:editor-error "Claude is not asking anything"))
    (pop (session-pending session))
    (js:funcall *claude-answer* (session-handle session) request
                (if allow t :false) (or message "") (if always t :false))
    (when (and allow always)
      (let ((suggestion (first (jlist request "request" "permission_suggestions"))))
        (when (jstring suggestion "mode")
          (note-mode session (jstring suggestion "mode")))))
    (write-chat session (if allow "  ✓ allowed" "  ✗ not allowed") (if allow :added :removed))
    (write-chat session (format nil "~@[: ~A~]~%" (and (not allow) (plusp (length message)) message)))
    (if (session-pending session)
        (write-permission-request session (first (session-pending session)))
        (restore-input session))))

;;; ---- what claude says

(defun write-user-turn (session text)
  (blank-chat-line session)
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (write-chat session "❯ " :bullet)
    (write-chat session (format nil "~A~%" (first lines)) :bold)
    (dolist (line (rest lines))
      (write-chat session (format nil "  ~A~%" line) :bold)))
  (write-chat session (string #\Newline)))

(defun handle-stream-event (session event)
  (let ((type (jstring event "type")))
    (cond ((equal type "message_start")
           (let ((id (jstring event "message" "id")))
             (when id (push id (session-streamed session)))))
          ((and (equal type "content_block_start")
                (equal (jstring event "content_block" "type") "text"))
           (fresh-chat-line session)
           (setf (session-in-text session) t))
          ((and (equal type "content_block_delta")
                (equal (jstring event "delta" "type") "text_delta"))
           (write-chat session (or (jstring event "delta" "text") "")))
          ((equal type "content_block_stop")
           (when (session-in-text session)
             (setf (session-in-text session) nil)
             (fresh-chat-line session))))))

(defun handle-assistant (session message)
  (let ((streamed (member (jstring message "message" "id") (session-streamed session) :test #'equal)))
    (dolist (content (jlist message "message" "content"))
      (let ((type (jstring content "type")))
        (cond ((equal type "text")
               (unless streamed
                 (fresh-chat-line session)
                 (write-chat session (jstring content "text"))
                 (fresh-chat-line session)))
              ((equal type "tool_use")
               (write-tool-use session content)))))))

(defun handle-user (session message)
  (dolist (content (jlist message "message" "content"))
    (when (equal (jstring content "type") "tool_result")
      (write-tool-result session content (jget message "tool_use_result")))))

(defun handle-result (session message)
  (setf (session-state session) :idle
        (session-cost session) (jget message "total_cost_usd"))
  (let ((ms (jget message "duration_ms"))
        (cost (jget message "total_cost_usd")))
    (fresh-chat-line session)
    (write-chat session (format nil "~@[~,1Fs~]~@[ · $~,4F~]~@[ · ~A~]~@[ · ~A~]~%"
                                (and (realp ms) (/ ms 1000.0))
                                (and (realp cost) cost)
                                (let ((model (session-model session)))
                                  (and model (plusp (length model)) model))
                                (unless (equal (jstring message "subtype") "success")
                                  (jstring message "subtype")))
                :mark)))

(defun handle-claude-message (session message)
  (let ((type (jstring message "type")))
    (cond ((equal type "stream_event")
           (handle-stream-event session (jget message "event")))
          ((equal type "assistant") (handle-assistant session message))
          ((equal type "user") (handle-user session message))
          ((equal type "result") (handle-result session message))
          ((and (equal type "system") (equal (jstring message "subtype") "init"))
           (unless (session-model session)
             (setf (session-model session) (or (jstring message "model") "")))
           (setf (session-session-id session) (jstring message "session_id"))
           (when (eq (session-state session) :starting)
             (setf (session-state session) :idle)))
          ((equal type "control_response")
           (handle-control-response session (jget message "response")))
          ((equal type "control_request")
           (when (equal (jstring message "request" "subtype") "can_use_tool")
             (setf (session-pending session) (append (session-pending session) (list message)))
             (when (eq message (first (session-pending session)))
               (write-permission-request session message))
             (lem:message "Claude asks to use ~A: y, a or n" (jstring message "request" "tool_name")))))
    (schedule-redraw session)))

(defun note-mode (session mode)
  "Claude's permission mode is MODE now."
  (unless (equal mode (session-mode session))
    (setf (session-mode session) mode)
    (fresh-chat-line session)
    (if (equal mode "bypassPermissions")
        (write-chat session (format nil "⚠ bypassPermissions: Claude runs every tool without asking~%") :removed)
        (write-chat session (format nil "Permission mode: ~A~%" mode) :mark)))
  (lem:message "Permission mode: ~A" mode))

(defun handle-control-response (session response)
  "Claude's answer to a control request of the session: a new permission
mode, or why it refused one."
  (cond ((equal (jstring response "subtype") "error")
         (let ((error (or (jstring response "error") "refused")))
           (fresh-chat-line session)
           (write-chat session (format nil "✗ ~A~%" error) :removed)
           (lem:message "Claude: ~A" error)))
        ((jstring response "response" "mode")
         (note-mode session (jstring response "response" "mode")))))

(defun handle-claude-exit (session code why)
  (setf (session-state session) :exited
        (session-pending session) '())
  (restore-input session)
  (blank-chat-line session)
  (write-chat session (format nil "Claude Code ended~@[ (~A)~].~%" (and (realp code) (/= code 0) code))
              :removed)
  (when (and (stringp why) (plusp (length why)))
    (write-fenced session "" why)))

;;; ---- starting

(lem:define-major-mode claude-chat-mode nil
    (:name "Claude"
     :keymap *claude-chat-mode-keymap*)
  (setf (lem:buffer-read-only-p (lem:current-buffer)) t))

(lem:define-major-mode claude-input-mode nil
    (:name "Prompt"
     :keymap *claude-input-mode-keymap*))

(defun mode-label (mode)
  "A short name of the permission mode MODE, for the modeline."
  (cond ((equal mode "default") "ask")
        ((equal mode "acceptEdits") "edits")
        ((equal mode "bypassPermissions") "⚠bypass")
        (t mode)))

(defun session-status (window)
  "The part of the modeline that tells how the session of WINDOW is."
  (let ((session (buffer-session (lem:window-buffer window))))
    (if session
        ;; (short: the window of the input is narrow)
        (format nil " ~A · ~A~@[ · $~,2F~] "
                (case (session-state session)
                  ;; (claude says it has started only after the first message)
                  (:responding (if (session-pending session) "asking" "working"))
                  (:exited "ended")
                  (t "ready"))
                (mode-label (session-mode session))
                (and (realp (session-cost session)) (session-cost session)))
        "")))

(defun project-directory (buffer)
  (let ((directory (or (lem:buffer-directory buffer) (namestring (user-homedir-pathname)))))
    (namestring (or (ignore-errors (lem-core/commands/project:find-root directory))
                    directory))))

(defun start-session (directory &key continue)
  (unless (js:funcall *node-available*)
    (lem:editor-error "Claude Code runs in the desktop application (it starts the claude command)"))
  (let* ((name (car (last (pathname-directory (uiop:ensure-directory-pathname directory)))))
         (chat (lem:make-buffer (format nil "*claude: ~A*" (or name directory))))
         (input (lem:make-buffer (format nil "*claude input: ~A*" (or name directory))))
         (session (make-claude-session :directory (uiop:ensure-directory-pathname directory)
                                       :chat chat :input input)))
    (dolist (buffer (list chat input))
      (setf (lem:buffer-directory buffer) (namestring directory)))
    (lem:change-buffer-mode chat 'claude-chat-mode)
    (lem:with-current-buffer chat
      (markdown-live-mode t))
    (lem:change-buffer-mode input 'claude-input-mode)
    ;; the input goes with the conversation: shown in a box at the bottom of
    ;; each window that shows it (an attached window has no modeline: the
    ;; status is in the conversation's)
    (lem:attach-buffer chat input)
    (lem:modeline-add-status-list 'session-status chat)
    (setf (session-bypass-allowed session) (and *claude-allow-bypass-permissions* t))
    (when *claude-permission-mode*
      (setf (session-mode session) *claude-permission-mode*))
    (push session *claude-sessions*)
    (write-chat session (format nil "Claude Code in ~A~%" directory) :mark)
    (when (equal *claude-permission-mode* "bypassPermissions")
      (write-chat session (format nil "⚠ bypassPermissions: Claude runs every tool without asking~%") :removed))
    (write-chat session (format nil "Return send · C-j newline · S-Tab mode · C-c C-c stop · C-c C-o conversation~%") :mark)
    (setf (session-handle session)
          (js:funcall *claude-spawn* *claude-command* (namestring directory)
                      (append '("-p" "--input-format" "stream-json" "--output-format" "stream-json"
                                "--verbose" "--include-partial-messages"
                                "--permission-prompt-tool" "stdio")
                              (and continue '("--continue"))
                              (and *claude-allow-bypass-permissions*
                                   '("--allow-dangerously-skip-permissions"))
                              (and *claude-permission-mode*
                                   (list "--permission-mode" *claude-permission-mode*))
                              *claude-arguments*)
                      (lambda (message)
                        (lem:send-event (lambda () (handle-claude-message session message)))
                        nil)
                      (lambda (code why)
                        (lem:send-event (lambda ()
                                          (handle-claude-exit session code why)
                                          (lem:redraw-display)))
                        nil)))
    (js:call (session-handle session) "control" "initialize" :false :false)
    session))

(defun input-window (chat-window session)
  "The input box of CHAT-WINDOW, a window of the conversation of SESSION."
  (or (lem-core::window-attached-window chat-window)
      (lem-core::make-attached-window chat-window :buffer (session-input session))))

(defun show-session (session)
  "Show the conversation, with its input in a box at its bottom, and go to
the input.  The current window shows it if it showed a conversation, or if
it is narrow; otherwise the conversation is put on its right."
  (let ((chat-window (first (lem:get-buffer-windows (session-chat session)))))
    (unless chat-window
      (let ((window (lem:current-window)))
        (when (lem:attached-window-p window)
          (setf window (lem-core::attached-window-parent-window window)))
        (lem:switch-to-window window)
        (when (and (not (buffer-session (lem:window-buffer window)))
                   (< 100 (lem:window-width window)))
          ;; side by side with what is edited
          (lem:split-window-horizontally window)
          (lem:switch-to-window (lem:get-next-window window)))
        (lem:switch-to-buffer (session-chat session))
        (setf chat-window (first (lem:get-buffer-windows (session-chat session))))))
    (lem:switch-to-window (input-window chat-window session))))

(defun live-session (directory)
  (find-if (lambda (session)
             (and (equal (namestring (session-directory session))
                         (namestring (uiop:ensure-directory-pathname directory)))
                  (not (eq (session-state session) :exited))))
           *claude-sessions*))

;;; ---- commands

(lem:define-command claude () ()
  "Talk with Claude Code about the project of the buffer (started if it is not)."
  (let* ((directory (project-directory (lem:current-buffer)))
         (session (or (buffer-session (lem:current-buffer))
                      (live-session directory)
                      (start-session directory))))
    (show-session session)))

(lem:define-command claude-continue () ()
  "Start Claude Code again with the last conversation of the project."
  (show-session (start-session (project-directory (lem:current-buffer)) :continue t)))

(lem:define-command claude-send () ()
  "Send the input to Claude."
  (let* ((session (current-session))
         (input (session-input session))
         (text (string-trim '(#\Space #\Newline #\Tab) (lem:buffer-text input))))
    (cond ((eq (session-state session) :exited)
           (lem:editor-error "Claude Code has ended: M-x claude starts it again"))
          ((session-pending session)
           (lem:editor-error "Claude asks first: y, a or n"))
          ((zerop (length text)))
          (t
           (lem:erase-buffer input)
           (write-user-turn session text)
           (setf (session-state session) :responding)
           (js:funcall *claude-send-user* (session-handle session) text)))))

(lem:define-command claude-interrupt () ()
  "Stop what Claude is doing."
  (let ((session (current-session)))
    (js:call (session-handle session) "control" "interrupt" :false :false)
    (dolist (request (session-pending session))
      (declare (ignore request))
      (answer-permission session nil :message "Interrupted."))
    (lem:message "Interrupted")))

(defun session-modes (session)
  "The permission modes that Shift-Tab cycles through in SESSION."
  (append *claude-permission-modes*
          (and (session-bypass-allowed session) '("bypassPermissions"))))

(lem:define-command claude-cycle-mode () ()
  "Change the permission mode: default (ask), acceptEdits, plan, and
bypassPermissions if the session allows it (*claude-allow-bypass-permissions*).
The mode shown changes when Claude has taken it."
  (let* ((session (current-session))
         (modes (session-modes session))
         (next (or (second (member (session-mode session) modes :test #'string=))
                   (first modes))))
    (js:call (session-handle session) "control" "set_permission_mode" "mode" next)
    (lem:message "Permission mode: ~A..." next)))

(lem:define-command claude-bypass-permissions () ()
  "Let Claude run every tool without asking (bypassPermissions), if the
session was started with *claude-allow-bypass-permissions*."
  (let ((session (current-session)))
    (unless (session-bypass-allowed session)
      (lem:editor-error "Set lem-clysm:*claude-allow-bypass-permissions* to T, then start Claude Code again (M-x claude-restart)"))
    (js:call (session-handle session) "control" "set_permission_mode" "mode" "bypassPermissions")))

(lem:define-command claude-restart () ()
  "End the Claude Code of the project and start it again with the last
conversation (with the options as they are now)."
  (let* ((session (current-session))
         (directory (namestring (session-directory session)))
         (windows (lem:get-buffer-windows (session-chat session))))
    (unless (eq (session-state session) :exited)
      (js:call (session-handle session) "kill"))
    (setf (session-state session) :exited)
    ;; the new conversation in the first window of the old one
    (dolist (window (rest windows))
      (unless (lem:one-window-p)
        (lem:delete-window window)))
    (when windows
      (lem:switch-to-window (first windows)))
    (show-session (start-session directory :continue t))))

(defun answer-key (allow always character)
  (let ((session (buffer-session (lem:current-buffer))))
    (cond ((and session (session-pending session))
           (if (< (- (get-internal-real-time) (session-asked-at session))
                  (* *claude-answer-delay* internal-time-units-per-second))
               ;; (typed for the input before the question came)
               (lem:message "Claude asks: y, a or n")
               (answer-permission session allow :always always
                                                :message (unless allow
                                                           (lem:prompt-for-string "Tell Claude what to do instead (or Return): ")))))
          ((eq (lem:current-buffer) (and session (session-input session)))
           (lem:insert-character (lem:current-point) character))
          (t (lem:editor-error "Claude is not asking anything")))))

(lem:define-command claude-yes () ()
  "Allow what Claude asks for."
  (answer-key t nil #\y))

(lem:define-command claude-always () ()
  "Allow what Claude asks for, and the same from now on."
  (answer-key t t #\a))

(lem:define-command claude-no () ()
  "Do not allow what Claude asks for (and tell it why)."
  (answer-key nil nil #\n))

(lem:define-command claude-send-region (start end) (:region)
  "Put the region, with where it is, into the input of Claude."
  (let* ((buffer (lem:current-buffer))
         (session (or (live-session (project-directory buffer))
                      (start-session (project-directory buffer))))
         (file (lem:buffer-filename buffer))
         (text (lem:points-to-string start end)))
    (let ((quoted (format nil "~@[In ~A, ~]lines ~D-~D:~%```~%~A~%```~%"
                          (and file (relative-path session (namestring file)))
                          (lem:line-number-at-point start) (lem:line-number-at-point end)
                          (string-right-trim '(#\Newline) text)))
          (draft (session-draft session)))
      (if draft
          ;; (Claude asks in the input: for what is typed, put back after)
          (setf (session-draft session)
                (cons (concatenate 'string (car draft) quoted)
                      (+ 1 (length (car draft)) (length quoted))))
          (lem:insert-string (lem:buffer-end-point (session-input session)) quoted)))
    (show-session session)
    (unless (session-draft session)
      (lem:buffer-end (lem:current-point)))))

(lem:define-command claude-focus-input () ()
  "Go to the input of Claude."
  (show-session (current-session)))

(lem:define-command claude-focus-chat () ()
  "Go to the conversation with Claude (from its input)."
  (let ((window (lem:current-window)))
    (if (lem:attached-window-p window)
        (lem:switch-to-window (lem-core::attached-window-parent-window window))
        (let ((chat (first (lem:get-buffer-windows (session-chat (current-session))))))
          (if chat
              (lem:switch-to-window chat)
              (lem:editor-error "The conversation is not shown: M-x claude"))))))

(lem:define-key *claude-input-mode-keymap* "Return" 'claude-send)
(lem:define-key *claude-input-mode-keymap* "C-j" 'lem-core/commands/edit:newline)
(lem:define-key *claude-input-mode-keymap* "M-Return" 'lem-core/commands/edit:newline)
(lem:define-key *claude-input-mode-keymap* "Shift-Tab" 'claude-cycle-mode)
(lem:define-key *claude-input-mode-keymap* "C-c C-c" 'claude-interrupt)
(lem:define-key *claude-input-mode-keymap* "C-c C-o" 'claude-focus-chat)
(lem:define-key *claude-input-mode-keymap* "y" 'claude-yes)
(lem:define-key *claude-input-mode-keymap* "a" 'claude-always)
(lem:define-key *claude-input-mode-keymap* "n" 'claude-no)
(lem:define-key *claude-chat-mode-keymap* "y" 'claude-yes)
(lem:define-key *claude-chat-mode-keymap* "a" 'claude-always)
(lem:define-key *claude-chat-mode-keymap* "n" 'claude-no)
(lem:define-key *claude-chat-mode-keymap* "C-c C-c" 'claude-interrupt)
(lem:define-key *claude-chat-mode-keymap* "Return" 'claude-focus-input)
(lem:define-key *claude-chat-mode-keymap* "i" 'claude-focus-input)
