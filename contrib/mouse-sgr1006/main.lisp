(uiop/package:define-package :lem-mouse-sgr1006/main
  (:nicknames :lem-mouse-sgr1006) (:use :cl :lem)
  (:shadow) (:export :parse-mouse-event
                     :*scroll-unfocused-window*)
  (:intern))
(in-package :lem-mouse-sgr1006/main)
;;;don't edit above
(defparameter *message-on-mouse-event* nil)
(defparameter *scroll-unfocused-window* nil)

(defvar *dragging-window* ())
(defvar *min-cols*  5)
(defvar *min-lines* 1)
(defvar *wheel-scroll-size* 3)

;; mouse button type
(defvar *mouse-button-1*   0)
(defvar *mouse-wheel-up*   64)
(defvar *mouse-wheel-down* 65)

(defun get-window-rect (window)
  (values (lem:window-x     window)
          (lem:window-y     window)
          (lem:window-width window)
          (- (lem:window-height window)
             (if (lem:window-use-modeline-p window) 1 0))))

(defun move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem:window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point)
                                   (- x (lem:window-left-width window))))

(defun all-window-list ()
  "returns a list of all windows including side windows"
  (let ((side-window (lem:frame-leftside-window (lem:current-frame)))
        (window-list (lem:window-list)))
    (if side-window
        (cons side-window window-list)
        window-list)))

(defun find-window-containing-pos (x1 y1)
  (or (find-if (lambda (o)
                 (multiple-value-bind (x y w h) (get-window-rect o)
                   (and (<= x x1 (+ x w)) (<= y y1 (+ y h)))))
               (all-window-list))
      (current-window)))

(defun parse-mouse-event (getch-fn)
  (let ((msg (loop :for c := (code-char (funcall getch-fn))
                   :with result
                   :with part
                   :until (or (char= c #\m)
                              (char= c #\M))
                   :when (char= c #\;)
                   :do (setq result #1=(cons (parse-integer (format nil "~{~A~}"
                                                                    (reverse part)))
                                             result)
                             part nil)
                   :else
                   :do (push c part)
                   :finally (return (cons c (reverse #1#))))))
    (lambda ()
      (multiple-value-bind (bstate btype x1 y1)
          (apply #'values msg)
        ;; convert mouse position from 1-origin to 0-origin
        (decf x1)
        (decf y1)
        ;; check mouse status
        (when (or (and (not (lem:floating-window-p (lem:current-window)))
                       (eql btype *mouse-button-1*)
                       (or (eql bstate #\m)
                           (eql bstate #\M)))
                  (and (or (eql btype *mouse-wheel-up*)
                           (eql btype *mouse-wheel-down*))
                       (eql bstate #\M)))
          ;; send a dummy key event to exit isearch-mode
          (lem:send-event (lem:make-key :sym "NopKey")))
        ;; send actual mouse event
        (lem:send-event (parse-mouse-event-sub bstate btype x1 y1))))))

(defun parse-mouse-event-sub (bstate btype x1 y1)
  (lambda ()
    ;; process mouse event
    (cond
      ;; button-1 down
      ((and (or (not (lem:floating-window-p (lem:current-window)))
                (lem:side-window-p (lem:current-window)))
            (eql btype *mouse-button-1*)
            (eql bstate #\M))
       (or
        ;; for frame header window
        (find-if
         (lambda (o)
           (multiple-value-bind (x y w h) (get-window-rect o)
             (cond
               ;; select frame
               ((and (<= x x1 (+ x w -1)) (<= y y1 (+ y h -1)))
                (lem:with-point ((point (lem:buffer-start-point (lem:window-buffer o))))
                  (when (lem:line-offset point (- y1 y) (- x1 x))
                    (let ((button (lem/button:button-at point)))
                      (when button
                        (lem/button:button-action button)
                        (lem:update-on-display-resized)))))
                t)
               (t nil))))
         (lem:frame-header-windows (lem:current-frame)))
        ;; for normal window
        (find-if
         (lambda (o)
           (multiple-value-bind (x y w h) (get-window-rect o)
             (cond
               ;; vertical dragging window
               ((and (= y1 (- y 1)) (<= x x1 (+ x w -1)))
                (setf *dragging-window* (list o 'y))
                t)
               ;; horizontal dragging window
               ((and (= x1 (- x 1)) (<= y y1 (+ y h -1)))
                (setf *dragging-window* (list o 'x))
                t)
               ;; move cursor
               ((and (<= x x1 (+ x w -1)) (<= y y1 (+ y h -1)))
                (setf (lem:current-window) o)
                (move-to-cursor o (- x1 x) (- y1 y))
                (lem:redraw-display)
                t)
               (t nil))))
         (all-window-list))))
      ;; button-1 up
      ((and (eql btype *mouse-button-1*)
            (eql bstate #\m))
       (let ((o-orig (lem:current-window))
             (o (first *dragging-window*)))
         (when (windowp o)
           (multiple-value-bind (x y w h) (get-window-rect o)
             (cond
               ;; vertical dragging window
               ((eq (second *dragging-window*) 'y)
                (let ((vy (- (- y 1) y1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (not (lem:floating-window-p (lem:current-window)))
                             (>= y1       *min-lines*)
                             (>= (+ h vy) *min-lines*))
                    (setf (lem:current-window) o)
                    (lem:grow-window vy)
                    (setf (lem:current-window) o-orig)
                    (lem:redraw-display))))
               ;; horizontal dragging window
               ((eq (second *dragging-window*) 'x)
                (let ((vx (- (- x 1) x1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (not (lem:floating-window-p (lem:current-window)))
                             (>= x1       *min-cols*)
                             (>= (+ w vx) *min-cols*))
                    (setf (lem:current-window) o)
                    (lem:grow-window-horizontally vx)
                    (setf (lem:current-window) o-orig)
                    (lem:redraw-display))))
               )))
         (when o
           (setf *dragging-window*
                 (list nil (list x1 y1) *dragging-window*)))))
      ;; wheel up
      ((and (eql btype *mouse-wheel-up*)
            (eql bstate #\M))
       (let ((target-window (if *scroll-unfocused-window*
                                (find-window-containing-pos x1 y1)
                                (current-window))))
         ;; As a command, through the major mode's EXECUTE (a terminal
         ;; buffer shows its history so), as src/mouse.lisp scrolls.
         (when target-window
           (with-current-window target-window
             (call-command 'scroll-up *wheel-scroll-size*)))
         (lem:redraw-display)))
      ;; wheel down
      ((and (eql btype *mouse-wheel-down*)
            (eql bstate #\M))
       (let ((target-window (if *scroll-unfocused-window*
                                (find-window-containing-pos x1 y1)
                                (current-window))))
         ;; As a command, through the major mode's EXECUTE (a terminal
         ;; buffer shows its history so), as src/mouse.lisp scrolls.
         (when target-window
           (with-current-window target-window
             (call-command 'scroll-down *wheel-scroll-size*)))
         (lem:redraw-display)))
      )
    (when *message-on-mouse-event*
      (lem:message "mouse:~s ~s ~s ~s" bstate btype x1 y1)
      (lem:redraw-display))))

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defun enable-hook ()
  (format lem-ncurses:*terminal-io-saved* "~A[?1000h~A[?1002h~A[?1006h~%" #\esc #\esc #\esc)
  (ignore-errors
    (dolist (window (lem:window-list))
      (lem:screen-clear (lem:window-screen window)))
    (lem:redraw-display))
  (lem:run-hooks *enable-hook*))

(defun disable-hook ()
  (format lem-ncurses:*terminal-io-saved* "~A[?1006l~A[?1002l~A[?1000l~%" #\esc #\esc #\esc)
  (ignore-errors
    (dolist (window (lem:window-list))
      (lem:screen-clear (lem:window-screen window)))
    (lem:redraw-display))
  (run-hooks *disable-hook*))

(define-minor-mode mouse-sgr-1006-mode
    (:global t
     :enable-hook #'enable-hook
     :disable-hook #'disable-hook))

(defun enable-mouse-sgr-1006-mode ()
  (mouse-sgr-1006-mode t))

(defun disable-mouse-sgr-1006-mode ()
  (mouse-sgr-1006-mode nil))

(add-hook *after-init-hook* 'enable-mouse-sgr-1006-mode)
(add-hook *exit-editor-hook* 'disable-mouse-sgr-1006-mode)

(eval-when (:load-toplevel)
  (enable-mouse-sgr-1006-mode))
