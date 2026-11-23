(in-package :lem-core)

(defun window-recenter (window &key line from-bottom)
  "Recenter WINDOW to the given LINE number.
LINE must be NIL or a positive number.
If LINE is NIL, recenter to the middle of the WINDOW.
Otherwise, recenter to the nth LINE (starting at 0), counted from the top.
If FROM-BOTTOM is T, start counting from the bottom."
  (check-type line (or null integer))
  (check-type from-bottom boolean)
  (setq line (cond ((null line)
                    (floor (window-height-without-modeline window) 2))
                   (from-bottom
                    (- (window-height-without-modeline window) line 1))
                   (t line)))
  (unless (= line (window-cursor-y window))
    (line-start
     (move-point (window-view-point window)
                 (window-buffer-point window)))
    (let ((n (- (window-cursor-y window) line)))
      (window-scroll window n)
      n)))

(defun window-recenter-top-bottom (window)
  "In first call recenter WINDOW to the middle line.
If cursor is already in the middle of WINDOW then move cursor in the top position.
If cursor is on top then move move WINDOW to the bottom."
  (let* ((line (window-cursor-y window))
         (window-height (window-height-without-modeline window))
         (middle (floor window-height 2))
         (scrolloff (min (floor (/ (window-height window) 2)) 0))
         (top 0))
    (cond
      ((= line middle) (window-recenter window :line scrolloff :from-bottom nil))
      ((= line top) (window-recenter window :line scrolloff :from-bottom t))
      (t (window-recenter window :line nil :from-bottom nil)))))

(defun window-text-measure (window)
  "Return how WINDOW's text is measured for wrapping, as values ADVANCE and
LIMIT.  ADVANCE is a function of a string, an index into it and an x
position, returning the x after that character, by grapheme cluster (see
string-char-width); a line wraps before the character that takes x past
LIMIT.  In columns for monospace text, and in the frontend's units (see
lem-if:char-pixel-width) for proportional text, where LIMIT matches the
display's own wrapping in separate-objects-by-width and only the text drawn
proportionally (see proportional-type) is measured by its glyphs: emoji and
wide characters keep their cells."
  (let* ((buffer (window-buffer window))
         (tab-size (variable-value 'tab-width :default buffer)))
    (if (proportional-text-p buffer)
        (let* ((implementation (implementation))
               (cell-width (lem-if:get-char-width implementation))
               (tab-width (* tab-size (lem-if:char-pixel-width implementation #\Space))))
          (values (lambda (string index x)
                    (let ((character (char string index)))
                      (cond ((char= character #\Tab)
                             (* (1+ (floor x tab-width)) tab-width))
                            ((and (member (char-type character) '(:latin :other))
                                  (not (grapheme-continuation-p string index)))
                             (+ x (lem-if:char-pixel-width implementation character)))
                            (t
                             (+ x (* cell-width (string-char-width string index 0)))))))
                  ;; Leave room for the wrap character, as
                  ;; separate-objects-by-width does.
                  (- (window-view-width window)
                     (* (+ (window-left-width window)
                           (char-width (variable-value 'wrap-line-character :default buffer) 0))
                        (lem-if:get-char-width implementation)))))
        (values (lambda (string index x)
                  (string-char-width string index x :tab-size tab-size))
                (1- (window-body-width window))))))

(defun %calc-window-cursor-x (point window)
  "Return (values cur-x next). the 'next' is a flag if the cursor goes to
next line because it is at the end of width."
  (unless (variable-value 'line-wrap :default (window-buffer window))
    (return-from %calc-window-cursor-x (values (point-column point) nil)))
  (multiple-value-bind (advance width) (window-text-measure window)
    (let* ((charpos (point-charpos point))
           (line    (line-string point))
           (cur-x   0)
           (add-x   (if (< charpos (length line))
                        (funcall advance line charpos 0)
                        1)))
      (loop :for i :from 0 :below charpos
            :do (setf cur-x (funcall advance line i cur-x))
                (when (< width cur-x)
                  (setf cur-x (funcall advance line i 0))))
      (if (< width (+ cur-x add-x))
          (values 0     t)
          (values cur-x nil)))))

(defun window-cursor-x (window)
  (multiple-value-bind (x next)
      (%calc-window-cursor-x (window-buffer-point window) window)
    (declare (ignore next))
    x))

(defun cursor-goto-next-line-p (point window)
  "Check if the cursor goes to next line because it is at the end of width."
  (multiple-value-bind (x next)
      (%calc-window-cursor-x point window)
    (declare (ignore x))
    next))

(defun wrap-index (string limit advance start)
  "Return the index of the first character of STRING from START that takes
the width past LIMIT, measured with ADVANCE (see window-text-measure)."
  (loop :with width := 0
        :for index :from start :below (length string)
        :do (setf width (funcall advance string index width))
            (when (< limit width)
              (return index))))

(defun map-wrapping-line (window string fn)
  (multiple-value-bind (advance limit) (window-text-measure window)
    (loop :with start := 0
          :for i := (wrap-index string limit advance start)
          :while i
          :do (funcall fn i)
              (setq start i))))

(defun window-wrapping-offset (window start-point end-point)
  (unless (variable-value 'line-wrap :default (window-buffer window))
    (return-from window-wrapping-offset 0))
  (let ((offset 0))
    (labels ((inc (arg)
               (declare (ignore arg))
               (incf offset)))
      (map-region start-point
                  end-point
                  (lambda (string lastp)
                    (declare (ignore lastp))
                    (map-wrapping-line window
                                       string
                                       #'inc)))
      offset)))

(defun count-hidden-lines (start-point end-point)
  "Number of hidden buffer lines between START-POINT and END-POINT."
  (when (point< end-point start-point)
    (rotatef start-point end-point))
  (with-point ((p start-point)
               (goal end-point))
    (line-start p)
    (line-start goal)
    (loop :with count := 0
          :until (same-line-p p goal)
          :do (unless (line-offset p 1)
                (return count))
              (when (line-continuation-p p)
                (incf count))
          :finally (return count))))

(defun window-cursor-y-not-wrapping (window)
  "Number of screen rows between the view point and the cursor.
excludes lines hidden by overlays with :invisible property because those lines dont occupy any
vertical space."
  (let ((view-point (window-view-point window))
        (buffer-point (window-buffer-point window)))
    (- (count-lines buffer-point view-point)
       (count-hidden-lines view-point buffer-point))))

(defun window-cursor-y (window)
  (if (point< (window-buffer-point window)
              (window-view-point window))
      ;; return minus number
      (- (+ (window-cursor-y-not-wrapping window)
            (window-wrapping-offset window
                                    (backward-line-wrap
                                     (copy-point (window-buffer-point window)
                                                 :temporary)
                                     window t)
                                    (window-view-point window))
            (if (cursor-goto-next-line-p (window-view-point window) window)
                1 0)))
      ;; return zero or plus number
      (+ (window-cursor-y-not-wrapping window)
         (window-wrapping-offset window
                                 (window-view-point window)
                                 (window-buffer-point window))
         (if (and (point< (window-view-point window)
                          (window-buffer-point window))
                  (cursor-goto-next-line-p (window-buffer-point window) window))
             1 0))))

(defun forward-line-wrap (point window)
  (assert (eq (point-buffer point) (window-buffer window)))
  (when (variable-value 'line-wrap :default (point-buffer point))
    (map-wrapping-line window
                       (line-string point)
                       (lambda (i)
                         (when (< (point-charpos point) i)
                           (line-offset point 0 i)
                           (return-from forward-line-wrap point))))))

(defun backward-line-wrap-1 (point window contain-same-line-p)
  (if (and contain-same-line-p (start-line-p point))
      point
      (let (previous-charpos)
        (map-wrapping-line window
                           (line-string point)
                           (lambda (i)
                             (cond ((and contain-same-line-p (= i (point-charpos point)))
                                    (line-offset point 0 i)
                                    (return-from backward-line-wrap-1 point))
                                   ((< i (point-charpos point))
                                    (setf previous-charpos i))
                                   (previous-charpos
                                    (line-offset point 0 previous-charpos)
                                    (return-from backward-line-wrap-1 point))
                                   ((or contain-same-line-p (= i (point-charpos point)))
                                    (line-start point)
                                    (return-from backward-line-wrap-1 point)))))
        (cond (previous-charpos
               (line-offset point 0 previous-charpos))
              (contain-same-line-p
               (line-start point))))))

(defun backward-line-wrap (point window contain-same-line-p)
  (assert (eq (point-buffer point) (window-buffer window)))
  (cond ((variable-value 'line-wrap :default (point-buffer point))
         (backward-line-wrap-1 point window contain-same-line-p))
        (contain-same-line-p
         (line-start point))))

(defun move-to-next-virtual-line-1 (point window)
  (assert (eq (point-buffer point) (window-buffer window)))
  (or (forward-line-wrap point window)
      (line-offset point 1)))

(defun move-to-previous-virtual-line-1 (point window)
  (assert (eq (point-buffer point) (window-buffer window)))
  (backward-line-wrap point window t)
  (or (backward-line-wrap point window nil)
      (progn
        (and (line-offset point -1)
             (line-end point)
             (backward-line-wrap point window t)))))

(defun move-to-next-virtual-line-n (point window n)
  (assert (eq (point-buffer point) (window-buffer window)))
  (when (<= n 0)
    (return-from move-to-next-virtual-line-n point))
  (unless (variable-value 'line-wrap :default (point-buffer point))
    (return-from move-to-next-virtual-line-n (line-offset point n)))
  (loop :with n1 := n
        :do (map-wrapping-line
             window
             (line-string point)
             (lambda (i)
               (when (< (point-charpos point) i)
                 (decf n1)
                 (when (<= n1 0)
                   ;; cursor-x offset is recovered by cursor-saved-column
                   (line-offset point 0 i)
                   (return-from move-to-next-virtual-line-n point)))))
            ;; go to next line
            (unless (line-offset point 1)
              (return-from move-to-next-virtual-line-n nil))
            (decf n1)
            (when (<= n1 0)
              (return-from move-to-next-virtual-line-n point))))

(defun move-to-previous-virtual-line-n (point window n)
  (assert (eq (point-buffer point) (window-buffer window)))
  (when (<= n 0)
    (return-from move-to-previous-virtual-line-n point))
  (unless (variable-value 'line-wrap :default (point-buffer point))
    (return-from move-to-previous-virtual-line-n (line-offset point (- n))))
  (let ((pos-ring  (make-array (1+ n))) ; ring buffer of wrapping position
        (pos-size  (1+ n))
        (pos-count 0)
        (pos-next  0)
        (pos-last  0))
    (flet ((pos-ring-push (pos)
             (setf (aref pos-ring pos-next) pos)
             (incf pos-next)
             (when (>= pos-next pos-size) (setf pos-next 0))
             (incf pos-count)
             (when (> pos-count pos-size)
               (setf pos-count pos-size)
               (incf pos-last)
               (when (>= pos-last pos-size) (setf pos-last 0)))))
      (loop :with n1 := n
            :with first-line := t
            :do (block outer
                  (pos-ring-push 0)
                  (map-wrapping-line
                   window
                   (line-string point)
                   (lambda (i)
                     (when (and first-line
                                (< (point-charpos point) i))
                       (return-from outer))
                     (pos-ring-push i))))
                (when (>= pos-count (1+ n1))
                  ;; cursor-x offset is recovered by cursor-saved-column
                  (line-offset point 0 (aref pos-ring pos-last))
                  (return-from move-to-previous-virtual-line-n point))
                ;; go to previous line
                (unless (line-offset point -1)
                  (return-from move-to-previous-virtual-line-n nil))
                (setf first-line nil)
                (decf n1 pos-count)
                (setf pos-size  (1+ n1)) ; shrink ring-buffer
                (setf pos-count 0)
                (setf pos-next  0)
                (setf pos-last  0)))))

(defun move-to-next-virtual-line (point &optional n (window (current-window)))
  (unless n (setf n 1))
  (unless (zerop n)

    ;; workaround for cursor movement problem
    (when (and *use-cursor-movement-workaround*
               (eq point (window-buffer-point window))
               (variable-value 'line-wrap :default (point-buffer point))
               ;; The saved column is a pixel x in proportional text.
               (not (proportional-text-p (point-buffer point)))
               (numberp (cursor-saved-column point))
               (>= (cursor-saved-column point) (- (window-body-width window) 3)))
      (setf (cursor-saved-column point) 0))

    (if *use-new-vertical-move-function*
        (if (plusp n)
            (move-to-next-virtual-line-n point window n)
            (move-to-previous-virtual-line-n point window (- n)))
        (multiple-value-bind (n f)
            (if (plusp n)
                (values n #'move-to-next-virtual-line-1)
                (values (- n) #'move-to-previous-virtual-line-1))
          (loop :repeat n
                :do (unless (funcall f point window)
                      (return-from move-to-next-virtual-line nil)))
          point))))

(defun move-to-previous-virtual-line (point &optional n (window (current-window)))
  (move-to-next-virtual-line point (if n (- n) -1) window))

(defun point-virtual-line-column (point &optional (window (current-window)))
  (when (proportional-text-p (point-buffer point))
    (return-from point-virtual-line-column
      (let ((advance (window-text-measure window))
            (line (line-string point)))
        (with-point ((start point))
          (backward-line-wrap start window t)
          (loop :with x := 0
                :for index :from (point-charpos start) :below (point-charpos point)
                :do (setf x (funcall advance line index x))
                :finally (return x))))))
  (if (variable-value 'line-wrap :default (point-buffer point))
      (let ((column (point-column point)))
        (with-point ((start point))
          (backward-line-wrap start window t)
          (- column (point-column start))))
      (point-column point)))

(defun move-to-virtual-line-column (point column &optional (window (current-window)))
  "Move POINT to COLUMN of its wrapped line: a column for monospace text, and
an x in the frontend's units for proportional text (see
window-text-measure), where POINT goes to the character boundary nearest
COLUMN."
  (backward-line-wrap point window t)
  (prog1 (move-to-virtual-line-column-1 point column window)
    ;; Never stop inside a grapheme cluster, e.g. between an emoji and its
    ;; skin tone.
    (skip-grapheme-continuations point)))

(defun inside-grapheme-cluster-p (point)
  "True if POINT is between two characters of one grapheme cluster, when
text is handled by cluster (see *grapheme-clusters*)."
  (and *grapheme-clusters*
       (not (end-line-p point))
       (grapheme-continuation-p (line-string point) (point-charpos point))))

(defun skip-grapheme-continuations (point)
  "Move POINT forward to the end of the grapheme cluster it is inside, if
any."
  (loop :while (inside-grapheme-cluster-p point)
        :do (character-offset point 1))
  point)

(defun grapheme-offset (point n)
  "Move POINT by N characters, as character-offset does, but by grapheme
cluster when *grapheme-clusters* is on: over a whole emoji sequence at once.
Return POINT, or NIL if the buffer ends first (POINT then stays at the end
it reached)."
  (if (not *grapheme-clusters*)
      (character-offset point n)
      (loop :repeat (abs n)
            :do (unless (character-offset point (if (plusp n) 1 -1))
                  (return nil))
                (if (plusp n)
                    (skip-grapheme-continuations point)
                    (loop :while (inside-grapheme-cluster-p point)
                          :do (character-offset point -1)))
            :finally (return point))))

(defun grapheme-character-count (point n)
  "Return how many characters the next N grapheme clusters from POINT span
(see grapheme-offset), stopping at the end of the buffer."
  (with-point ((end point))
    (grapheme-offset end n)
    (count-characters point end)))

(defun move-to-virtual-line-column-1 (point column window)
  (if (proportional-text-p (point-buffer point))
      (let ((advance (window-text-measure window)))
        (loop :with w := 0
              :while (< w column)
              :do (when (end-line-p point) (return nil))
                  (let ((next (funcall advance (line-string point) (point-charpos point) w)))
                    (when (< (- column w) (- next column))
                      (return t))
                    (setf w next)
                    (character-offset point 1))
              :finally (return t)))
      (let ((w 0))
        (loop
          :while (< w column)
          :do (when (end-line-p point) (return nil))
              (setf w (string-char-width (line-string point) (point-charpos point) w))
              (character-offset point 1)
          :finally (return t)))))

(defun window-scroll-down (window)
  (move-to-next-virtual-line (window-view-point window) 1 window))

(defun window-scroll-up (window)
  (move-to-previous-virtual-line (window-view-point window) 1 window))

(defun window-scroll-down-n (window n)
  (move-to-next-virtual-line (window-view-point window) n window))

(defun window-scroll-up-n (window n)
  (move-to-previous-virtual-line (window-view-point window) n window))

(defun view-detachment (window)
  "Return what scroll-view recorded about WINDOW's cursor and buffer when it
last scrolled the view away from the cursor, or NIL."
  (window-parameter window 'view-detachment))

(defun view-detached-p (window)
  "True if WINDOW's view was scrolled with scroll-view and its cursor has not
moved, nor its buffer changed, since: the redisplay then leaves the view
where it is instead of bringing it back to the cursor.  Once either has, the
detachment ends."
  (let ((detachment (view-detachment window))
        (buffer (window-buffer window)))
    (when detachment
      (destructuring-bind (detached-buffer position tick) detachment
        (or (and (eq detached-buffer buffer)
                 (= position (position-at-point (window-buffer-point window)))
                 (= tick (buffer-modified-tick buffer)))
            (progn
              (setf (window-parameter window 'view-detachment) nil)
              nil))))))

(defun scroll-view (window n)
  "Scroll WINDOW's view by N virtual lines (negative scrolls up), leaving its
cursor where it is in the text, even off screen, as a mouse wheel or
trackpad scrolls in most GUI editors.  The view stays there until the cursor
moves or the buffer changes; then the redisplay brings it back to the
cursor.  The scroll commands, by contrast, move the cursor to keep it on
screen.  Return true if the view moved all N lines."
  (let ((moved (window-scroll window n)))
    (unless moved
      (if (plusp n)
          (progn (buffer-end (window-view-point window))
                 (backward-line-wrap (window-view-point window) window t))
          (buffer-start (window-view-point window))))
    (setf (window-parameter window 'view-detachment)
          (list (window-buffer window)
                (position-at-point (window-buffer-point window))
                (buffer-modified-tick (window-buffer window))))
    moved))

(defun window-scroll (window n)
  (need-to-redraw window)
  (prog1 (if *use-new-vertical-move-function*
             (if (plusp n)
                 (window-scroll-down-n window n)
                 (window-scroll-up-n window (- n)))
             (dotimes (_ (abs n))
               (if (plusp n)
                   (window-scroll-down window)
                   (window-scroll-up window))))
    (run-hooks *window-scroll-functions* window)))
