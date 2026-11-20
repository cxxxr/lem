;;;; click.lisp — where a click lands, from what is drawn
;;;;
;;;; Lem finds the place of a click by counting the text of the buffer from
;;;; the top of the window.  In markdown-live-mode (markdown.lisp) the text
;;;; is not drawn as it is: its marks are hidden, some replaced (• for -, │
;;;; and padding in tables), headings are larger (wider, and two lines
;;;; high), images several lines high.  Lem then put the cursor away from
;;;; where one clicked, more so lower in the window.  Here the lines of the
;;;; window are laid out again as Lem draws them (its logical lines, drawing
;;;; objects and wrapping, with the widths and heights of this frontend),
;;;; knowing for each character drawn the character of the buffer it shows.

(in-package :lem-clysm)

;;; ---- the characters drawn -> the characters of the buffer

(defun splice-ops (vstart vend overlays length)
  "The text of the visual line VSTART..VEND (LENGTH characters) that OVERLAYS
hide or replace, as Lem's CREATE-LOGICAL-LINE takes it: (start end
replacement) in the characters of the line, sorted."
  (let ((ops '()))
    (dolist (overlay overlays)
      (let ((invisible (lem:overlay-get overlay :invisible))
            (display (lem:overlay-get overlay :display)))
        (when (and (or invisible display)
                   (lem:point<= (lem:overlay-start overlay) vend)
                   (lem:point<= vstart (lem:overlay-end overlay)))
          (let ((start (if (lem:point<= vstart (lem:overlay-start overlay))
                           (lem:count-characters vstart (lem:overlay-start overlay))
                           0))
                (end (if (lem:point<= (lem:overlay-end overlay) vend)
                         (lem:count-characters vstart (lem:overlay-end overlay))
                         length))
                (replacement (cond (display
                                    (let ((d (alexandria:ensure-list display)))
                                      (if (stringp (first d)) (first d) "")))
                                   ((eq invisible :ellipsis) "...")
                                   (t ""))))
            (when (< start end)
              (push (list start end replacement) ops))))))
    (sort ops #'< :key #'first)))

(defun drawn-sources (string ops tab-width)
  "For each character of STRING as it is drawn (OPS applied, then the tabs
expanded), the index in STRING of the character it shows (the start of
what a replacement replaces); and one more, the length of STRING."
  (let ((spliced '())
        (sources (make-array (1+ (length string)) :adjustable t :fill-pointer 0)))
    (loop :with i := 0
          :while (< i (length string))
          :do (let ((op (first ops)))
                (cond ((and op (<= (first op) i))
                       (pop ops)
                       (loop :for char :across (third op)
                             :do (push (cons char (first op)) spliced))
                       (setf i (max i (second op))))
                      (t
                       (push (cons (char string i) i) spliced)
                       (incf i)))))
    (loop :with column := 0
          :for (char . source) :in (nreverse spliced)
          :do (if (char= char #\Tab)
                  (let ((n (- tab-width (mod column tab-width))))
                    (loop :repeat n :do (vector-push-extend source sources))
                    (incf column n))
                  (progn
                    (vector-push-extend source sources)
                    (incf column))))
    (vector-push-extend (length string) sources)
    sources))

(defun add-virtual-sources (sources virtual-items)
  "SOURCES with the text that VIRTUAL-ITEMS (before or after an overlay)
put before the characters at their charpos: it shows the character there."
  (if (null virtual-items)
      sources
      (let ((result (make-array (length sources) :adjustable t :fill-pointer 0)))
        (loop :for i :from 0 :below (length sources)
              :do (loop :while (and virtual-items
                                    (<= (lem-core::virtual-item-charpos (first virtual-items)) i))
                        :do (loop :repeat (length (lem-core::virtual-item-string (pop virtual-items)))
                                  :do (vector-push-extend (aref sources i) result)))
                  (vector-push-extend (aref sources i) result))
        result)))

;;; ---- the drawing objects and the lines they take

(defun drawing-pieces (logical-line)
  "The drawing objects of LOGICAL-LINE (as Lem makes them to draw it), each
(object start length): the characters of the line drawn that it shows."
  (let ((lem-core::*line-wrap* t)
        (start 0)
        (pieces '()))
    (multiple-value-bind (items line-end-item)
        (lem-core::compute-items-from-logical-line logical-line)
      (dolist (item items)
        (let ((objects (lem-core::create-drawing-object item)))
          (if (or (typep item 'lem-core::eol-cursor-item)
                  (typep item 'lem-core::extend-to-eol-item))
              (dolist (object objects)
                (push (list object start 0) pieces))
              (let* ((string (lem-core::item-string item))
                     (attribute (lem-core::item-attribute item))
                     (image (and attribute
                                 (lem:attribute-image (lem:ensure-attribute attribute nil)))))
                (cond ((or image (zerop (length string)))
                       (dolist (object objects)
                         (push (list object start (length string)) pieces)))
                      (t
                       ;; an object for each run of characters of a kind
                       (loop :for object :in objects
                             :for (nil . part) :in (lem-core::split-string-by-character-type string)
                             :for offset := start :then (+ offset length)
                             :for length := (length part)
                             :do (push (list object offset length) pieces))))
                (incf start (length string))))))
      (when line-end-item
        (dolist (object (lem-core::create-drawing-object line-end-item))
          (push (list object start 0) pieces))))
    (nreverse pieces)))

(defun halve-piece (piece)
  "The two pieces Lem cuts the text object of PIECE into, to wrap it."
  (destructuring-bind (object start length) piece
    (let* ((string (lem-core/display:text-object-string object))
           (n (floor (length string) 2))
           (type (lem-core::char-type (char string 0)))
           (attribute (lem-core/display:text-object-attribute object))
           (first (lem-core::make-object-with-type (subseq string 0 n) attribute type))
           (second (lem-core::make-object-with-type (subseq string n) attribute type)))
      ;; (a control character is drawn as two characters, ^A)
      (if (= length (length string))
          (list (list first start n) (list second (+ start n) (- length n)))
          (list (list first start 0) (list second start length))))))

(defun take-row (pieces width)
  "The pieces of the first line of a window WIDTH wide that PIECES take,
each (x object start length), and the pieces left, as Lem's
SEPARATE-OBJECTS-BY-WIDTH cuts them."
  (loop :with total := 0
        :and row := '()
        :for piece := (pop pieces)
        :while piece
        :do (let* ((object (first piece))
                   (width-of (lem-core::object-width object)))
              (cond ((and (typep object 'lem-core/display:text-object)
                          (<= width (+ total width-of)))
                     (if (< 1 (length (lem-core/display:text-object-string object)))
                         (setf pieces (nconc (halve-piece piece) pieces))
                         (progn
                           (push piece pieces)
                           (return (values (nreverse row) pieces)))))
                    (t
                     (push (cons total piece) row)
                     (incf total width-of))))
        :finally (return (values (nreverse row) nil))))

(defun left-side-objects (logical-line)
  "The objects drawn left of the text of LOGICAL-LINE (line numbers...)."
  (let ((content (lem-core::logical-line-left-content logical-line)))
    (when content
      (mapcan #'lem-core::create-drawing-object
              (lem-core::compute-items-from-string-and-attributes
               (lem/buffer/line:content-string content)
               (lem/buffer/line:content-attributes content))))))

(defun visual-line-end (vstart overlays)
  "The end of the visual line from VSTART: past the newlines that
invisible overlays (folds) hide."
  (let ((invisible (remove-if-not (lambda (overlay) (lem:overlay-get overlay :invisible))
                                  overlays))
        (vend (lem:copy-point vstart :temporary)))
    (lem:line-end vend)
    (loop :until (lem:last-line-p vend)
          :while (lem-core::invisible-overlay-covering vend invisible)
          :do (lem:line-offset vend 1)
              (lem:line-end vend))
    vend))

(defun lay-out-line (point logical-line overlays tab-width width)
  "The lines of the window that LOGICAL-LINE, of the visual line from POINT,
takes, each (height . pieces); the sources of its characters drawn (see
DRAWN-SOURCES); and the start of its visual line."
  (let* ((vstart (lem:line-start (lem:copy-point point :temporary)))
         (vend (visual-line-end vstart overlays))
         (string (lem-core::collect-visual-line-string-and-attributes vstart vend))
         (ops (splice-ops vstart vend overlays (length string)))
         (sources (drawn-sources string ops tab-width))
         ;; a window scrolled into a wrapped line starts in its middle
         (clip (min (1- (length sources))
                    (lem-core::adjust-charpos-for-splices (lem:point-charpos point) ops)))
         (sources (add-virtual-sources (subseq sources clip)
                                       (lem-core::logical-line-virtual-items logical-line)))
         (left (left-side-objects logical-line))
         (text-width (- width (reduce #'+ left :key #'lem-core::object-width)))
         (rows '()))
    (loop :with pieces := (drawing-pieces logical-line)
          :do (multiple-value-bind (row rest) (take-row pieces text-width)
                (when (null row) (return))
                (push (cons (max 1 (reduce #'max (append left (mapcar #'second row))
                                           :key #'lem-core::object-height :initial-value 1))
                            row)
                      rows)
                (setf pieces rest)))
    (values (nreverse rows) sources vstart)))

;;; ---- the click

(defun source-point (vstart sources index)
  (let ((point (lem:copy-point vstart :temporary)))
    (lem:character-offset point (aref sources (max 0 (min index (1- (length sources))))))
    point))

(defun point-in-row (row last-row-p sources vstart x)
  "The point of the character drawn at column X of ROW (pieces), and whether
X is on it (not past the end of the line)."
  (loop :for (x0 object start length) :in row
        :for width := (lem-core::object-width object)
        :when (< x (+ x0 width))
          :do (return-from point-in-row
                (values (source-point vstart sources
                                      (if (< 1 length)
                                          (+ start (min (1- length)
                                                        (floor (- x x0) (/ width length))))
                                          start))
                        (plusp length))))
  ;; past the end: the end of the line, or the last character of a line
  ;; that wraps
  (values (source-point vstart sources
                        (if last-row-p
                            (1- (length sources))
                            (1- (reduce #'max row :key (lambda (piece) (+ (third piece) (fourth piece)))
                                                  :initial-value 1))))
          nil))

(defun drawn-point-at (window x y)
  "The point of the character that WINDOW draws at column X (a fraction of
a column: the middle of a cell) of its line Y, and whether X is on the text."
  (let* ((buffer (lem:window-buffer window))
         (overlays (lem-core::get-window-overlays window))
         (active-modes (lem-core::get-active-modes-class-instance buffer))
         (lem-core::*active-modes* active-modes)
         (width (lem-core::window-view-width window))
         (tab-width (lem:variable-value 'lem:tab-width :default buffer))
         (top 0)
         (last nil))
    (flet ((in-row (row more sources vstart)
             (point-in-row row (null more) sources vstart x)))
      (lem:with-point ((point (lem:window-view-point window)))
        (loop
          (let ((logical-line (lem-core::create-logical-line point overlays active-modes)))
            (multiple-value-bind (rows sources vstart)
                (lay-out-line point logical-line overlays tab-width width)
              (loop :for ((height . row) . more) :on rows
                    :do (setf last (list row more sources vstart))
                        (when (< y (+ top height))
                          (return-from drawn-point-at (in-row row more sources vstart)))
                        (incf top height))))
          ;; the next visual line (the lines a fold hides are passed)
          (loop
            (unless (lem:line-offset point 1)
              ;; under the end of the buffer: its last line
              (return-from drawn-point-at (and last (apply #'in-row last))))
            (unless (lem-core::line-continuation-p point)
              (return))))))))

(defmethod lem-if:point-at-window-position ((implementation clysm) window x y)
  ;; only where the text is drawn otherwise than it is: elsewhere Lem counts
  ;; it right
  (let ((buffer (lem:window-buffer window)))
    (when (and (lem:mode-active-p buffer 'markdown-live-mode)
               (lem:variable-value 'lem:line-wrap :default buffer))
      (drawn-point-at window (+ x 1/2) y))))
