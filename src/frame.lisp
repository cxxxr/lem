(in-package :lem-core)

(defun make-display-frame-map ()
  "Return a table for *display-frame-map*: safe to read from one thread while
another writes it."
  ;; CCL's and LispWorks' hash tables are thread-safe already.
  (make-hash-table #+(or sbcl ecl) :synchronized #+(or sbcl ecl) t))

(defvar *display-frame-map* (make-display-frame-map)
  "The frame shown on each display.  The frontends read it from their own
threads (SDL's event loop) while the editor thread maps and unmaps frames.")
(defvar *frames* '())

(defvar *frame-buffer-list-function* nil
  "NIL, or a function of one argument, a frame, that returns the buffers
to offer for selection in that frame.
Extensions that use each frame as a separate workspace, such as
frame-multiplexer, set this.  A non-empty list restricts the frame to those
buffers.  To restrict it to no buffer at all, return NIL and T as a second
value: only T, so that a function that happens to return another second value
(as GETHASH or FIND-SYMBOL do) does not hide every buffer.  When the variable
is NIL, or the function returns NIL without T, every buffer is offered.")

(defgeneric update-prompt-window (window)
  (:method (window)))

(defclass frame ()
  ((window-left-margin
    :initarg :window-left-margin
    :reader frame-window-left-margin)
   (enable-modeline-per-window
    :initarg :enable-modeline-per-window
    :initform t
    :reader frame-enable-window-modeline-per-window)
   (current-window
    :initarg :current-window
    :initform nil
    :accessor frame-current-window)
   (window-tree
    :initarg :window-tree
    :initform nil
    :accessor frame-window-tree)
   (floating-windows
    :initarg :floating-windows
    :initform '()
    :accessor frame-floating-windows)
   (header-windows
    :initarg :header-windows
    :initform '()
    :reader frame-header-windows)
   (modified-floating-windows
    :initform nil
    :reader frame-modified-floating-windows
    :writer set-frame-modified-floating-windows
    :documentation "このスロットがTの場合、
floating-windowが追加/削除/大きさの変更などがあったことを示します。
redraw-display関数でこのスロットを参照して、必要なウィンドウの再描画を判断するために使います。
またredraw-display内でNILにセットされます。
このスロットは内部処理のためのものであり、使ってはいけません。(DO NOT USE THIS SLOT)")
   (modified-header-windows
    :initform nil
    :reader frame-modified-header-windows
    :writer set-frame-modified-header-windows
    :documentation "このスロットがTの場合、header-windowが追加または削除されたことを示します。
redraw-display関数でこのスロットを参照して、画面の再描画時にウィンドウの大きさを
計算仕直す必要があるか判断します。
またredraw-display内でNILにセットされます。
このスロットは内部処理のためのものであり、使ってはいけません。(DO NOT USE THIS SLOT)")
   (require-redisplay-windows
    :initform nil
    :reader frame-require-redisplay-windows
    :writer set-frame-require-redisplay-windows
    :documentation "このスロットがTの場合、
redraw-display関数でキャッシュを捨てて画面全体を再描画します。

このスロットが使われるケース
- frame-multiplexer-delete呼び出し後にredraw-displayで再描画時に画面の最小限の更新だけでは
  変更前のフレームが残ってしまう問題がありキャッシュを無効化する必要がある
- インアクティブなウィンドウの背景色を変える場合、
  ウィンドウ切り替え時などに画面全体を再描画する必要がある

このスロットは内部処理のためのものであり、使ってはいけません。(DO NOT USE THIS SLOT)")
   (prompt-window
    :initform nil
    :accessor frame-floating-prompt-window)
   (message-window
    :initform nil
    :accessor frame-message-window)
   (leftside-window
    :initform nil
    :accessor frame-leftside-window)
   (rightside-window
    :initform nil
    :accessor frame-rightside-window)
   (bottomside-window
    :initform nil
    :accessor frame-bottomside-window)))

(defmethod frame-window-bottom-margin ((frame frame))
  (if (frame-enable-window-modeline-per-window frame)
      0
      (window-bottom-margin (implementation))))

(defmethod notify-floating-window-modified ((frame frame))
  (set-frame-modified-floating-windows t frame))

(defmethod notify-header-window-modified ((frame frame))
  (set-frame-modified-header-windows t frame))

(defmethod notify-frame-redisplay-required ((frame frame))
  (set-frame-require-redisplay-windows t frame))

(defmethod notify-frame-redraw-finished ((frame frame))
  (set-frame-modified-header-windows nil frame)
  (set-frame-modified-floating-windows nil frame)
  (set-frame-require-redisplay-windows nil frame))

(defmethod frame-prompt-window ((frame frame))
  (frame-floating-prompt-window frame))

(defmethod frame-caller-of-prompt-window ((frame frame))
  (caller-of-prompt-window (frame-prompt-window frame)))

(defmethod frame-prompt-active-p ((frame frame))
  (alexandria:when-let (prompt (frame-prompt-window frame))
    (prompt-active-p prompt)))

(defun make-frame (&optional (old-frame (current-frame)))
  (let ((frame (make-instance 'frame
                              :window-left-margin (window-left-margin (implementation))
                              :enable-modeline-per-window t)))
    (push frame *frames*)
    (when old-frame
      (dolist (window (frame-header-windows old-frame))
        (add-header-window frame window)))
    frame))

(defun map-frame (display frame)
  (setf (gethash display *display-frame-map*) frame))

(defun get-frame (display)
  (gethash display *display-frame-map*))

(defun all-frames ()
  *frames*)

(defun current-frame ()
  (get-frame (implementation)))

(defun current-frame-ready-p ()
  "True when there is a current frame and it has its window tree.
Not before setup-first-frame has built it, nor after exit, once teardown has
unmapped the frame: frontends that handle window events on a thread of their
own (SDL2, SDL3) check this before laying out the windows.
The frame is read once, as the editor thread may unmap it meanwhile."
  (let ((frame (current-frame)))
    (and frame (frame-window-tree frame) t)))

(defun frame-buffer-list (&optional (frame (current-frame)))
  "Return the buffers to offer for selection in FRAME, most recently used first.
This is what buffer completion, list-buffers, next-buffer and previous-buffer use,
so that a frame used as a workspace only shows its own buffers.
Lookups by name or file (get-buffer, get-file-buffer) still see every buffer:
two workspaces visiting the same file share one buffer.
The list is empty when FRAME is restricted to its own buffers and none of them
is left; callers that need a buffer must handle that."
  (multiple-value-bind (buffers restricted)
      (when (and *frame-buffer-list-function* frame)
        (funcall *frame-buffer-list-function* frame))
    (if (or buffers (eq restricted t))
        buffers
        (buffer-list))))

(defun unmap-frame (display)
  (let ((frame (gethash display *display-frame-map*)))
    (remhash display *display-frame-map*)
    frame))

(defun setup-frame (frame buffer)
  (setup-frame-windows frame buffer))

(defun teardown-frame (frame)
  "Forget FRAME and free its windows: it leaves all-frames, and a display it is
mapped to has no frame afterwards (current-frame is NIL there) rather than a
frame whose windows were freed.  It is unmapped first, so that a frontend
thread that checks current-frame-ready-p does not lay out windows being freed."
  (alexandria:deletef *frames* frame)
  (let ((displays '()))
    (maphash (lambda (display mapped)
               (when (eq mapped frame)
                 (push display displays)))
             *display-frame-map*)
    (dolist (display displays)
      (remhash display *display-frame-map*)))
  (teardown-windows frame))

(defun map-protected (function list)
  "Call FUNCTION on each element of LIST, on all of them even if one signals.
The first error is not caught: it is signalled where it happens, with its
stack.  While it unwinds, FUNCTION is called on the rest of LIST, and their
errors are logged (log:error), not signalled: a condition signalled then
would replace the first error, or let a handler that exits skip the rest.
Only errors are contained so: other conditions FUNCTION signals pass through."
  (loop :for (element . rest) :on list
        :do (let ((done nil))
              (unwind-protect
                   (progn
                     (funcall function element)
                     (setf done t))
                (unless done
                  (dolist (other rest)
                    (handler-case (funcall function other)
                      (error (condition)
                        ;; Reporting CONDITION may fail too (its report reads
                        ;; what was torn down).
                        (ignore-errors
                         (log:error "~A also failed: ~A" other condition))))))))))

(defun teardown-frames ()
  "Tear down every frame: the ones made (all-frames), such as the hidden tabs of
frame-multiplexer, and the ones mapped to a display.
Each frame is torn down even if an earlier one signals an error."
  (let ((frames (copy-list (all-frames))))
    (maphash (lambda (display frame)
               (declare (ignore display))
               (pushnew frame frames))
             *display-frame-map*)
    (map-protected #'teardown-frame frames)))

(defun window-in-frame-p (window frame)
  (when (or (find window (window-list frame))
            (find window (frame-floating-windows frame))
            (find window (frame-header-windows frame))
            (eq window (frame-leftside-window frame))
            (eq window (frame-rightside-window frame))
            (eq window (frame-bottomside-window frame)))
    t))

(defun get-frame-of-window (window)
  ;; TODO: frameのリストを用意し、その中から探すようにする
  (when (window-in-frame-p window (current-frame))
    (current-frame)))

(defun update-floating-prompt-window (frame)
  (when (frame-floating-prompt-window frame)
    (update-prompt-window (frame-floating-prompt-window frame))))


(defun add-floating-window (frame window)
  (alexandria:nconcf (frame-floating-windows frame)
                     (list window)))

(defun remove-floating-windows (frame window)
  (alexandria:deletef (frame-floating-windows frame)
                      window))

(defun add-header-window (frame window)
  (with-slots (header-windows) frame
    (alexandria:nconcf header-windows (list window))))

(defun remove-header-window (frame window)
  (with-slots (header-windows) frame
    (setf header-windows (remove window header-windows))))


(defun topleft-window-y (frame)
  "Return the Y coordinate where the topmost editor window begins.
This is the sum of all header window heights in FRAME."
  (loop :for w :in (frame-header-windows frame)
        :sum (header-window-height w)))

(defun topleft-window-x (frame)
  (if (null (frame-leftside-window frame))
      0
      (1+ (window-width (frame-leftside-window frame)))))

(defun max-window-width (frame)
  (- (display-width)
     (topleft-window-x frame)
     (if (frame-rightside-window frame)
         (+ (window-width (frame-rightside-window frame))
            1 ; border
            )
         0)))

(defun max-window-height (frame)
  (- (display-height)
     (topleft-window-y frame)
     (if (frame-bottomside-window frame)
         (window-height (frame-bottomside-window frame))
         0)))


(defun within-window-p (window x y)
  (check-type x integer)
  (check-type y integer)
  (and (<= (window-x window) x (+ (window-x window) (window-width window) -1))
       (<= (window-y window) y (+ (window-y window) (window-height window) -1))))

(defun focus-window-position (frame x y)
  (check-type x integer)
  (check-type y integer)
  (dolist (window (append (frame-header-windows frame)
                          ;; リストの後ろにあるウィンドウほど手前に出てくるという前提
                          (reverse (frame-floating-windows frame))
                          (window-list frame)))
    (dolist (target (list (window-attached-window window) window))
      (when (and target (within-window-p target x y))
        (let ((overlay-x-offset (window-left-width target)))
          (return-from focus-window-position
            (values target
                    (- x (window-x target) overlay-x-offset)
                    (- y (window-y target)))))))))

(defun focus-separator-position (frame x y)
  (when (and (frame-leftside-window frame)
             (= x (window-width (frame-leftside-window frame))))
    (return-from focus-separator-position (values :leftside (frame-leftside-window frame))))
  (when (and (frame-rightside-window frame)
             (= x (1- (window-x (frame-rightside-window frame)))))
    (return-from focus-separator-position (values :rightside (frame-rightside-window frame))))
  (dolist (window (window-list frame))
    (when (and (= x (1- (window-x window)))
               (<= (window-y window) y (+ (window-y window) (window-height window) -1)))
      (return (values :vertical (left-window window) window)))
    (when (and (window-use-modeline-p window)
               (<= (window-x window) x (+ (window-x window) (window-width window) -1))
               (= y (+ (window-y window) (window-height window) -1)))
      (return (values :horizontal (down-window window) window)))))
