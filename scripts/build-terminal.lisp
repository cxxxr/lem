;;;; Build the lem-terminal native helper (terminal.so) for the current
;;;; platform from extensions/terminal/terminal.c.
;;;;
;;;; Requires a C compiler and libvterm. On Linux and FreeBSD, also
;;;; requires libutil (for forkpty); macOS provides it via libc.
;;;;
;;;; Invoke via `make terminal-lib` or directly:
;;;;   sbcl --script scripts/build-terminal.lisp
;;;;
;;;; The resulting .so is written under
;;;;   extensions/terminal/lib/<os>/<arch>/terminal.so
;;;; which matches the path lem-terminal/ffi.lisp pushes onto
;;;; cffi:*foreign-library-directories*.

(require :asdf)
(require :uiop)

(asdf:load-asd
 (merge-pathnames "extensions/terminal/lem-terminal.asd"
                  (uiop:getcwd)))

(defun terminal-source ()
  (asdf:system-relative-pathname :lem-terminal "terminal.c"))

(defun detect-architecture ()
  "Match ffi.lisp's runtime path resolution. Falls back to *features*
when uiop:architecture returns NIL (older bundled UIOP on Apple Silicon)."
  (or (uiop:architecture)
      (cond ((member :arm64 *features*) :arm64)
            ((member :aarch64 *features*) :arm64)
            ((member :x86-64 *features*) :x64)
            ((member :x86 *features*) :x86)
            (t (error "Could not detect architecture from *features*: ~A"
                      *features*)))))

(defun terminal-lib ()
  (asdf:system-relative-pathname
   :lem-terminal
   (format nil "~(lib/~A/~A/terminal.so~)"
           (uiop:operating-system)
           (detect-architecture))))

(defun static-p ()
  "True when LEM_TERMINAL_STATIC requests a self-contained terminal.so with
libvterm linked statically. Used by the release bundles (AppImage, macOS app)
so the shared library carries no external libvterm dependency. Source installs
leave this unset and dynamic-link against the system libvterm."
  (let ((v (uiop:getenv "LEM_TERMINAL_STATIC")))
    (and v (plusp (length v))
         (not (member v '("0" "no" "false") :test #'string-equal)))))

(defun libvterm-prefix ()
  "Resolve the libvterm install prefix. Honors LIBVTERM_PREFIX, then a
platform default: on macOS `brew --prefix libvterm` or the default Homebrew
location; on FreeBSD LOCALBASE or /usr/local."
  (or (uiop:getenv "LIBVTERM_PREFIX")
      #+darwin (ignore-errors
                (let ((p (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (uiop:run-program '("brew" "--prefix" "libvterm")
                                                        :output :string))))
                  (when (plusp (length p)) p)))
      #+darwin "/opt/homebrew/opt/libvterm"
      #+freebsd (or (uiop:getenvp "LOCALBASE") "/usr/local")))

;; libvterm linker fragment, selected by platform and static/dynamic mode.
;; macOS dynamic uses Homebrew's include/lib; macOS static links the archive
;; directly (no -L/-lvterm, which would re-introduce a dynamic dependency).
;; Linux static wraps -lvterm in -Bstatic so only libvterm is static while
;; libutil/libc stay dynamic.
;; FreeBSD links dynamically against (libvterm-prefix) (LOCALBASE, default
;; /usr/local) with an rpath so non-default prefixes load at runtime.
(defun vterm-flags ()
  (if (static-p)
      #+darwin (let ((prefix (libvterm-prefix)))
                 (format nil "-I~A/include ~A/lib/libvterm.a" prefix prefix))
      #-darwin "-Wl,-Bstatic -lvterm -Wl,-Bdynamic -lutil"
      #+darwin "-I/opt/homebrew/include -L/opt/homebrew/lib -lvterm"
      #+freebsd (let ((prefix (libvterm-prefix)))
                  (format nil "-I~A/include -L~A/lib -Wl,-rpath,~A/lib -lvterm -lutil"
                          prefix prefix prefix))
      #-(or darwin freebsd) "-lvterm -lutil"))

(defun build-command (source lib)
  (let ((cc (or (uiop:getenv "CC") "cc"))
        (cflags (or (uiop:getenv "CFLAGS") ""))
        (ldflags (or (uiop:getenv "LDFLAGS") "")))
    (format nil "~A ~A ~A ~A ~A -o ~A -shared -fPIC"
            cc cflags (namestring source) (vterm-flags) ldflags
            (namestring lib))))

(defun build-failed (format-control &rest format-arguments)
  "Report a build failure on stderr and exit non-zero so callers (make,
CI) can detect it. lem-terminal silently disables itself at runtime when
the shared library is missing, so the hint below is the only feedback a
user gets that the terminal extension is unavailable."
  (format *error-output* "~&Failed to build terminal.so: ")
  (apply #'format *error-output* format-control format-arguments)
  (format *error-output*
          "~&The lem-terminal extension will silently disable itself at~%~
           runtime when the shared library is missing. Install libvterm~%~
           (and a C toolchain) to enable it.~%")
  (finish-output *error-output*)
  (uiop:quit 1))

(defun build-terminal ()
  (let* ((source (terminal-source))
         (lib (terminal-lib))
         (cmd (build-command source lib)))
    (unless (probe-file source)
      (build-failed "terminal.c not found at ~A~%" source))
    (ensure-directories-exist lib)
    ;; Drop any stale artifact so a compiler that exits 0 without writing
    ;; the file can't masquerade as a successful build below.
    (when (probe-file lib)
      (delete-file lib))
    (format t "~&Building lem-terminal native helper:~%  ~A~%" cmd)
    (handler-case
        (uiop:run-program cmd :output t :error-output t)
      (uiop:subprocess-error (c)
        (build-failed "~A~%" c)))
    (unless (probe-file lib)
      (build-failed "compiler reported success but ~A was not produced.~%" lib))
    (format t "~&Wrote ~A~%" lib)
    (finish-output)
    lib))

(build-terminal)

;; Exit explicitly so `sbcl --load` does not drop into the REPL (and block
;; on stdin) after a successful build.
(uiop:quit 0)
