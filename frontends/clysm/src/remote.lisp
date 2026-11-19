;;;; remote.lisp — looking at the editor from outside, to find what is wrong
;;;;
;;;; M-x remote-start makes the desktop application answer HTTP on
;;;; 127.0.0.1 (a server of Node.js in the page), so that a program on the
;;;; computer (frontends/clysm/tools/lem-remote, which an agent like Claude
;;;; Code can run) sees the editor as it is and acts in it:
;;;;
;;;;   GET  /screen   the text of each window on the screen, the cursor
;;;;   GET  /state    the windows, the buffers, the message of the echo area
;;;;   GET  /log      what was written to the console (errors...), latest last
;;;;   POST /eval     the Lisp forms of the body, evaluated in the editor
;;;;   POST /keys     the keys of the body (as "C-x o"), typed
;;;;
;;;; Where it answers is in remote.json of Lem's home (LEM_HOME, ~/.lem/ or
;;;; ~/.config/lem/), readable by the user only: the port and a token that
;;;; each request gives (Authorization: Bearer TOKEN).  Requests of web pages
;;;; (with an Origin) are refused.  M-x remote-stop ends it.

(in-package :lem-clysm)

;;; what the page writes to the console, kept from its start
(sys::%defvar-host *console-log*
  (js:eval "(() => {
     const log = [];
     const keep = (kind) => (...args) => {
       const text = args.map((a) => (a && a.stack) ? a.stack : (typeof a === 'string' ? a : (() => { try { return JSON.stringify(a); } catch { return String(a); } })())).join(' ');
       log.push(new Date().toISOString().slice(11, 23) + ' ' + kind + ' ' + text);
       if (log.length > 400) log.splice(0, log.length - 400);
     };
     for (const kind of ['error', 'warn']) {
       const original = console[kind].bind(console);
       const note = keep(kind);
       console[kind] = (...args) => { note(...args); original(...args); };
     }
     if (typeof window === 'object') {
       window.addEventListener('error', (e) => keep('uncaught')(e.error || e.message));
       window.addEventListener('unhandledrejection', (e) => keep('unhandled')(e.reason));
     }
     return log;
   })()"))

(sys::%defvar-host *remote-listen*
  (js:eval "(token, file, handle, started) => {
     const http = require('node:http'), fs = require('node:fs'), path = require('node:path');
     const server = http.createServer((req, res) => {
       const reply = (status, text) => {
         res.writeHead(status, { 'content-type': 'text/plain; charset=utf-8' });
         res.end(text);
       };
       if (req.headers.origin) return reply(403, 'no requests of web pages\\n');
       if (req.headers.authorization !== 'Bearer ' + token) return reply(401, 'a token is needed: see remote.json\\n');
       let body = '';
       req.setEncoding('utf8');
       req.on('data', (d) => { body += d; });
       req.on('end', () => handle(req.method, req.url.split('?')[0], body, reply));
     });
     server.on('error', (e) => started(null, String(e.message)));
     server.listen(0, '127.0.0.1', () => {
       const port = server.address().port;
       fs.mkdirSync(path.dirname(file), { recursive: true });
       fs.writeFileSync(file, JSON.stringify({ url: 'http://127.0.0.1:' + port, token, pid: process.pid }) + '\\n', { mode: 0o600 });
       fs.chmodSync(file, 0o600);
       started(port, null);
     });
     return server;
   }"))

(sys::%defvar-host *remote-close*
  (js:eval "(server, file) => { server.close(); try { require('node:fs').unlinkSync(file); } catch {} }"))

(sys::%defvar-host *random-token*
  (js:eval "() => require('node:crypto').randomBytes(24).toString('hex')"))

(defvar *remote-server* nil "The HTTP server of M-x remote-start, or NIL.")

(defun remote-file ()
  (namestring (merge-pathnames "remote.json" (lem:lem-home))))

;;; ---- the screen as text

(defun segments-text (segments width)
  "The text of a line of SEGMENTS, WIDTH columns wide: each character in
its columns (2 for a wide one, more for larger text), as on the screen."
  (let ((cells (make-array (max 0 width) :initial-element #\Space)))
    (dolist (segment segments)
      (destructuring-bind (x w text &optional style class image scale) segment
        (declare (ignore w style class))
        (let ((text (if image "[image]" text))
              (scale (or scale 1))
              (column x)
              (free 0))
          (loop for char across text
                for columns = (* (lem:string-width (string char)) scale)
                ;; (smaller text: each character in a column of its own all the same)
                for start = (max (floor column) free)
                do (setf free (1+ start))
                   (when (< -1 start (length cells))
                     (setf (aref cells start) char)
                     ;; (the other columns of the character are not characters)
                     (loop for i from (1+ start) below (min (length cells) (ceiling (+ column columns)))
                           do (setf (aref cells i) nil)))
                   (incf column columns)))))
    (string-right-trim " " (coerce (remove nil (coerce cells 'list)) 'string))))

(defun view-text (view)
  (with-output-to-string (out)
    (let ((window (view-window view))
          (heights (view-heights view))
          (covered-until 0))
      (format out "== ~(~A~) ~S at ~D,~D ~Dx~D~:[~; (current)~]~%"
              (view-role view) (lem:buffer-name (lem:window-buffer window))
              (view-x view) (view-y view) (view-width view) (view-height view)
              (eq window (lem:current-window)))
      (loop for segments across (view-lines view)
            for y from 0
            for height = (if (< y (length heights)) (svref heights y) 1)
            when (<= covered-until y)
              do (format out "~A~%" (segments-text segments (view-width view)))
                 (setf covered-until (+ y height)))
      (when (and (view-modeline-p view) (view-modeline view))
        (format out "-- ~A~%" (string-trim " " (segments-text (view-modeline view) (view-width view))))))))

(defun screen-text ()
  (format nil "~{~A~}cursor at column ~D, line ~D of the screen~%"
          (mapcar #'view-text *views*) (first *cursor*) (second *cursor*)))

;;; ---- the state

(defun echo-area-text ()
  (let ((window (ignore-errors (lem:frame-message-window (lem:current-frame)))))
    (and window (lem:buffer-text (lem:window-buffer window)))))

(defun state-text ()
  (with-output-to-string (out)
    (format out "windows:~%")
    (dolist (window (lem:window-list))
      (let ((point (lem:window-point window)))
        (format out "  ~:[ ~;*~] ~S line ~D column ~D, from line ~D~%"
                (eq window (lem:current-window))
                (lem:buffer-name (lem:window-buffer window))
                (lem:line-number-at-point point) (lem:point-column point)
                (lem:line-number-at-point (lem:window-view-point window)))))
    (format out "buffers:~%")
    (dolist (buffer (lem:buffer-list))
      (format out "  ~S~@[ ~A~]~:[~; (modified)~] ~A~{ ~A~}~%"
              (lem:buffer-name buffer)
              (lem:buffer-filename buffer)
              (lem:buffer-modified-p buffer)
              (lem:mode-name (lem:buffer-major-mode buffer))
              (mapcar (lambda (mode) (or (ignore-errors (lem:mode-name mode)) mode))
                      (lem:buffer-minor-modes buffer))))
    (format out "echo area: ~S~%" (echo-area-text))))

(defun log-text ()
  (let ((log *console-log*))
    (format nil "~{~A~%~}"
            (loop for i below (or (js:get log "length") 0)
                  collect (js:get log i)))))

;;; ---- acting

(defun eval-text (text)
  "Evaluate the forms of TEXT in the editor (in LEM-USER): what they wrote,
then their values; (values text status)."
  (let ((*package* (or (find-package "LEM-USER") *package*))
        (output (make-string-output-stream)))
    (handler-case
        (let ((values '()))
          (with-input-from-string (in text)
            (let ((*standard-output* output))
              (loop for form = (read in nil in)
                    until (eq form in)
                    do (setf values (multiple-value-list (eval form))))))
          (values (format nil "~A~{~S~%~}" (get-output-stream-string output) values) 200))
      (error (condition)
        (values (format nil "~A~A: ~A~%" (get-output-stream-string output)
                        (type-of condition) condition)
                500)))))

(defun handle-remote (method path body reply)
  (flet ((answer (text &optional (status 200))
           (js:funcall reply status text)))
    (cond ((and (equal method "GET") (equal path "/screen")) (answer (screen-text)))
          ((and (equal method "GET") (equal path "/state")) (answer (state-text)))
          ((and (equal method "GET") (equal path "/log")) (answer (log-text)))
          ((and (equal method "POST") (equal path "/eval"))
           ;; in the editor, between its commands
           (lem:send-event (lambda ()
                             (multiple-value-bind (text status) (eval-text body)
                               (lem:redraw-display)
                               (answer text status)))))
          ((and (equal method "POST") (equal path "/keys"))
           (handler-case
               (let ((keys (lem:parse-keyspec (string-trim '(#\Space #\Newline) body))))
                 (dolist (key keys) (lem:send-event key))
                 (answer (format nil "~D keys~%" (length keys))))
             (error (condition) (answer (format nil "~A~%" condition) 400))))
          (t (answer (format nil "~A ~A: GET /screen /state /log, POST /eval /keys~%" method path) 404))))
  nil)

;;; ---- commands

(lem:define-command remote-start () ()
  "Answer HTTP on 127.0.0.1 so that programs of the computer see the editor
and act in it (remote.json of Lem's home tells them where, with a token)."
  (unless (js:funcall *node-available*)
    (lem:editor-error "The desktop application answers, not a browser's page"))
  (when *remote-server*
    (lem:editor-error "Already answering: ~A" (remote-file)))
  (let ((file (remote-file)))
    (setf *remote-server*
          (js:funcall *remote-listen* (js:funcall *random-token*) file
                      (lambda (method path body reply)
                        (handle-remote method path body reply))
                      (lambda (port error)
                        (lem:send-event (lambda ()
                                          (if port
                                              (lem:message "Answering on 127.0.0.1:~A (~A)" port file)
                                              (progn (setf *remote-server* nil)
                                                     (lem:message "Cannot answer: ~A" error)))))
                        nil)))))

(lem:define-command remote-stop () ()
  "Stop answering HTTP (M-x remote-start)."
  (when *remote-server*
    (js:funcall *remote-close* *remote-server* (remote-file))
    (setf *remote-server* nil)
    (lem:message "Stopped answering")))
