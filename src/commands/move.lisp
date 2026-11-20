(defpackage :lem-core/commands/move
  (:use :cl :lem-core)
  (:export :next-line
           :next-logical-line
           :previous-line
           :previous-logical-line
           :forward-char
           :backward-char
           :move-to-beginning-of-buffer
           :move-to-end-of-buffer
           :move-to-beginning-of-line
           :move-to-beginning-of-logical-line
           :move-to-end-of-line
           :move-to-end-of-logical-line
           :visual-line-beginning
           :visual-line-end
           :next-page
           :previous-page
           :next-page-char
           :previous-page-char
           :goto-line)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/move)

(define-key *global-keymap* "C-n" 'next-line)
(define-key *global-keymap* "Down" 'next-line)
(define-key *global-keymap* "C-p" 'previous-line)
(define-key *global-keymap* "Up" 'previous-line)
(define-key *global-keymap* "C-f" 'forward-char)
(define-key *global-keymap* "Right" 'forward-char)
(define-key *global-keymap* "C-b" 'backward-char)
(define-key *global-keymap* "Left" 'backward-char)
(define-key *global-keymap* "M-<" 'move-to-beginning-of-buffer)
(define-key *global-keymap* "C-Home" 'move-to-beginning-of-buffer)
(define-key *global-keymap* "M->" 'move-to-end-of-buffer)
(define-key *global-keymap* "C-End" 'move-to-end-of-buffer)
(define-key *global-keymap* "C-a" 'move-to-beginning-of-line)
(define-key *global-keymap* "Home" 'move-to-beginning-of-line)
(define-key *global-keymap* "C-e" 'move-to-end-of-line)
(define-key *global-keymap* "End" 'move-to-end-of-line)
(define-key *global-keymap* "C-v" 'next-page)
(define-key *global-keymap* "PageDown" 'next-page)
(define-key *global-keymap* "M-v" 'previous-page)
(define-key *global-keymap* "PageUp" 'previous-page)
(define-key *global-keymap* "C-x ]" 'next-page-char)
(define-key *global-keymap* "C-x [" 'previous-page-char)
(define-key *global-keymap* "M-g" 'goto-line)

(defvar *cursor-positions-before-command*
  (make-hash-table :test 'eq)
  "Maps each cursor point to its absolute buffer position before the running command, so
`run-overlay-cursor-motion-hooks' can tell which direction it moved.")

(defvar *cursor-overlays-before-command*
  (make-hash-table :test 'eq)
  "Maps each cursor point to the cursor-hook overlays it occupied before the running command,
so `run-overlay-cursor-motion-hooks' can tell which overlays it entered and which it left.")

(add-hook *pre-command-hook* 'snapshot-cursor-state)
(add-hook *post-command-hook* 'run-overlay-cursor-motion-hooks)

(defun snapshot-cursor-state ()
  "Snapshot every cursor's position and the cursor-hook overlays it occupies, so that after the
command `run-overlay-cursor-motion-hooks' can tell which overlays each cursor entered and left,
and in which direction."
  (clrhash *cursor-positions-before-command*)
  (clrhash *cursor-overlays-before-command*)
  (dolist (point (buffer-cursors (current-buffer)))
    (setf (gethash point *cursor-positions-before-command*)
          (position-at-point point))
    (setf (gethash point *cursor-overlays-before-command*)
          (overlays-with-cursor-hooks-covering point))))

(defun run-overlay-cursor-enter-functions (point direction)
  "Run the :cursor-enter-functions of every cursor-hook overlay POINT has entered since the
command, each called as (FUNCTION point overlay direction). repeats while a handler
repositions POINT so that overlays it is then pushed into also fire."
  (let ((seen (copy-list (gethash point *cursor-overlays-before-command*)))
        (visited (list (position-at-point point))))
    (loop
      (let ((entered (set-difference (overlays-with-cursor-hooks-covering point) seen)))
        (when (null entered)
          (return))
        (let ((before-pass (position-at-point point)))
          (dolist (overlay entered)
            (push overlay seen)
            (dolist (function (overlay-get overlay :cursor-enter-functions))
              (funcall function point overlay direction)))
          (let ((after-pass (position-at-point point)))
            ;; stop once a pass leaves POINT put, or revisits a position. the latter guards
            ;; against two overlays snapping a cursor back and forth forever.
            ;; this is tricky. this is definitely not the best way to do things but it works for now.
            ;; the nature of overlay hooks itself is tricky anyway.
            (when (or (= before-pass after-pass)
                      (member after-pass visited))
              (return))
            (push after-pass visited)))))))

(defun run-overlay-cursor-leave-functions (point direction)
  "Run the :cursor-leave-functions of every cursor-hook overlay POINT was in before the
command, each called as (FUNCTION point overlay direction)."
  (let ((left (set-difference (gethash point *cursor-overlays-before-command*)
                              (overlays-with-cursor-hooks-covering point))))
    (dolist (overlay left)
      (dolist (function (overlay-get overlay :cursor-leave-functions))
        (funcall function point overlay direction)))))

(defun cursor-move-direction-overlay (point)
  "The direction POINT moved during the command, :forward or :backward (:forward when its prior
position is unknown or unchanged)."
  (let ((before (gethash point *cursor-positions-before-command*)))
    (if (and before (< (position-at-point point) before))
        :backward
        :forward)))

(defun run-overlay-cursor-motion-hooks ()
  (dolist (point (buffer-cursors (current-buffer)))
    (let ((direction (cursor-move-direction-overlay point)))
      (run-overlay-cursor-enter-functions point direction)
      (run-overlay-cursor-leave-functions point direction))))

(defun visual-line-end (point)
  "Move POINT to the end of its visual line. returns POINT."
  (line-end point)
  (loop :until (last-line-p point)
        :while (invisible-overlay-covering point)
        :do (line-offset point 1)
            (line-end point))
  point)

(defun visual-line-beginning (point)
  "Move POINT to the start of its visual line. returns POINT."
  (line-start point)
  (loop :while (line-continuation-p point)
        :do (line-offset point -1)
            (line-start point))
  point)

(defun line-continuation-to-skip (point)
  (let ((overlay (line-continuation-p point)))
    (and overlay
         (not (overlay-get overlay :cursor-leave-functions))
         overlay)))

(defun skip-invisible-lines (point direction)
  "Move POINT past any fully invisible lines in DIRECTION (1 or -1).
returns POINT on success, or NIL if a buffer boundary is reached."
  (loop :while (line-continuation-to-skip point)
        :do (unless (line-offset point direction)
              (return-from skip-invisible-lines nil)))
  point)

(defun move-to-next-visible-virtual-line (point n)
  "Like `move-to-next-virtual-line' but skips fully invisible lines."
  (let ((dir (if (plusp n) 1 -1))
        (steps (abs n)))
    (loop :repeat steps
          :do (unless (move-to-next-virtual-line point dir)
                (return-from move-to-next-visible-virtual-line nil))
              (when (line-continuation-to-skip point)
                (unless (skip-invisible-lines point dir)
                  (return-from move-to-next-visible-virtual-line nil))))
    point))

(defun visible-line-offset (point n)
  "Like `line-offset' but skips fully invisible lines."
  (let ((dir (if (plusp n) 1 -1))
        (steps (abs n)))
    (loop :repeat steps
          :do (unless (line-offset point dir)
                (return-from visible-line-offset nil))
              (when (line-continuation-to-skip point)
                (unless (skip-invisible-lines point dir)
                  (return-from visible-line-offset nil))))
    point))

(defun next-line-aux (n
                      point-column-fn
                      forward-line-fn
                      move-to-column-fn)
  (if (continue-flag :next-line)
      (unless (not (null (cursor-saved-column (current-point))))
        (log:error "asseriton error: (not (null (cursor-saved-column (current-point))))"))
      (setf (cursor-saved-column (current-point))
            (funcall point-column-fn (current-point))))
  (unless (prog1 (funcall forward-line-fn (current-point) n)
            (funcall move-to-column-fn (current-point) (cursor-saved-column (current-point))))
    (cond ((plusp n)
           (move-to-end-of-buffer)
           (error 'end-of-buffer :point (current-point)))
          ((minusp n)
           (move-to-beginning-of-buffer)
           (error 'beginning-of-buffer :point (current-point))))))

(define-command (next-line (:advice-classes movable-advice)) (&optional n) (:universal)
  "Move the cursor to next line."
  (next-line-aux n
                 #'point-virtual-line-column
                 #'move-to-next-visible-virtual-line
                 #'move-to-virtual-line-column))

(define-command (next-logical-line (:advice-classes movable-advice)) (&optional n) (:universal)
  "Move the cursor to the next logical line."
  (next-line-aux n
                 #'point-column
                 #'visible-line-offset
                 #'move-to-column))

(define-command (previous-line (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous line."
  (next-line (- n)))

(define-command (previous-logical-line (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous logical line."
  (next-logical-line (- n)))

(define-command (forward-char (:advice-classes movable-advice))
    (&optional (n 1)) (:universal)
  "Move the cursor to the next character."
  (or (grapheme-offset (current-point) n)
      (error 'end-of-buffer :point (current-point))))

(define-command (backward-char (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous character."
  (or (grapheme-offset (current-point) (- n))
      (error 'beginning-of-buffer :point (current-point))))

(define-command (move-to-beginning-of-buffer (:advice-classes jump-cursor-advice)) () ()
  "Move the cursor to the beginning of the buffer."
  (run-hooks *set-location-hook* (current-point))
  (buffer-start (current-point)))

(define-command (move-to-end-of-buffer (:advice-classes jump-cursor-advice)) () ()
  "Move the cursor to the end of the buffer."
  (run-hooks *set-location-hook* (current-point))
  (buffer-end (current-point)))

(define-command (move-to-beginning-of-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the beginning of the line."
  (let ((bol (backward-line-wrap (copy-point (current-point) :temporary)
                                 (current-window)
                                 t)))
    (cond ((text-property-at (current-point) :field -1))
          ((previous-single-property-change (current-point)
                                            :field
                                            bol))
          ((start-line-p bol)
           (back-to-indentation bol)
           (if (point= bol (current-point))
               (line-start (current-point))
               (move-point (current-point) bol)))
          (t (move-point (current-point) bol)))))

(define-command (move-to-beginning-of-logical-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the beginning of the logical line."
  (line-start (current-point)))

(define-command (move-to-end-of-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the end of the line."
  (or (and (forward-line-wrap (current-point) (current-window))
           (character-offset (current-point) -1))
      (line-end (current-point))))

(define-command (move-to-end-of-logical-line (:advice-classes movable-advice)) () ()
  "Move the cursor to the end of the logical line."
  (line-end (current-point)))

(define-command (next-page (:advice-classes movable-advice)) (&optional n) (:universal-nil)
  "Move the cursor to the next page by one page."
  (if n
      (scroll (current-window) n)
      (progn
        (next-line (1- (window-height (current-window))))
        (window-recenter (current-window)))))

(define-command (previous-page (:advice-classes movable-advice)) (&optional n) (:universal-nil)
  "Move the cursor to the previous page by one page."
  (if n
      (scroll (current-window) (- n))
      (progn
        (previous-line (1- (window-height (current-window))))
        (window-recenter (current-window)))))

(define-command (next-page-char (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the next page character (^L)."
  (let ((point (current-point)))
    (dotimes (_ (abs n))
      (loop
        (unless (line-offset point (if (plusp n) 1 -1))
          (return-from next-page-char))
        (when (eql #\page (character-at point 0))
          (return))))))

(define-command (previous-page-char (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move the cursor to the previous page character (^L)."
  (next-page-char (- n)))

(define-command (goto-line (:advice-classes jump-cursor-advice)) (n) ((:number "Line to GOTO: "))
  "Move the cursor to the specified line number."
  (cond ((< n 1)
         (setf n 1))
        ((< #1=(buffer-nlines (current-buffer)) n)
         (setf n #1#)))
  (run-hooks *set-location-hook* (current-point))
  (line-offset (buffer-start (current-point)) (1- n)))
