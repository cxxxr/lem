(in-package :lem-core)

(defvar *active-modes*)

(defstruct virtual-item
  "a display-only string fragment injected at a character position within a logical line."
  ;; 0-based position in the line's string where this fragment is inserted
  charpos
  string
  attribute)

(defstruct logical-line
  string
  attributes
  virtual-items
  left-content
  end-of-line-cursor-attribute
  extend-to-end
  line-end-overlay)

(defun overlay-within-point-p (overlay point)
  (or (point<= (overlay-start overlay)
               point
               (overlay-end overlay))
      (same-line-p (overlay-start overlay)
                   point)
      (same-line-p (overlay-end overlay)
                   point)))

(defun expand-tab (string attributes tab-width)
  (setf attributes (copy-tree attributes))
  (if (not (find #\tab string))
      (values string attributes)
      ;; TODO: optimize
      (values (with-output-to-string (out)
                (loop :with i := 0
                      :for c :across string
                      :do (cond ((char= c #\tab)
                                 (let ((n (- tab-width (mod i tab-width))))
                                   (loop :for elt :in attributes
                                         :do (cond ((< i (first elt))
                                                    (incf (first elt) (1- n))
                                                    (incf (second elt) (1- n)))
                                                   ((and (< i (second elt))
                                                         (not (cursor-attribute-p (third elt))))
                                                    (incf (second elt) (1- n)))))
                                   (loop :repeat n
                                         :do (write-char #\space out))
                                   (incf i n)))
                                (t
                                 (write-char c out)
                                 (incf i)))))
              attributes)))

(defun overlay-attributes (under-attributes over-start over-end over-attribute)
  ;; under-attributes := ((start-charpos end-charpos attribute) ...)
  (let* ((over-attribute (ensure-attribute over-attribute))
         (under-part-attributes (lem/buffer/line:subseq-elements under-attributes
                                                               over-start
                                                               over-end))
         (merged-attributes (lem/buffer/line:remove-elements under-attributes
                                                           over-start
                                                           over-end)))
    (flet ((add-element (start end attribute)
             (when (< start end)
               (push (list start end (ensure-attribute attribute))
                     merged-attributes))))
      (if (null under-part-attributes)
          (add-element over-start over-end over-attribute)
          (loop :for prev-under := 0 :then under-end-offset
                :for (under-start-offset under-end-offset under-attribute)
                :in under-part-attributes
                :do (add-element (+ over-start prev-under)
                                 (+ over-start under-start-offset)
                                 over-attribute)
                    (add-element (+ over-start under-start-offset)
                                 (+ over-start under-end-offset)
                                 (alexandria:if-let (under-attribute
                                                     (ensure-attribute under-attribute nil))
                                   (merge-attribute under-attribute
                                                    over-attribute)
                                   over-attribute))
                :finally (add-element (+ over-start under-end-offset)
                                      over-end
                                      over-attribute))))
    (lem/buffer/line:normalization-elements merged-attributes)))

(defun splice-string (string attributes ov-start ov-end replacement replacement-attr)
  "replace [OV-START, OV-END) in STRING with REPLACEMENT, adjusting ATTRIBUTES."
  (let* ((rep-len (length replacement))
         (delta (- rep-len (- ov-end ov-start)))
         (new-string (str:concat (subseq string 0 ov-start)
                                 replacement
                                 (subseq string ov-end)))
         (pruned (lem/buffer/line:remove-elements attributes ov-start ov-end))
         (shifted (if (zerop delta)
                      pruned
                      (loop :for (start end attr) :in pruned
                            :collect (if (>= start ov-end)
                                         (list (+ start delta) (+ end delta) attr)
                                         (list start end attr)))))
         (final (if (and replacement-attr (plusp rep-len))
                    (lem/buffer/line:put-elements shifted ov-start (+ ov-start rep-len) replacement-attr)
                    shifted)))
    (values new-string final)))

(defun adjust-charpos-for-splices (charpos splice-ops)
  "Map a raw-string CHARPOS to its position after SPLICE-OPS are applied.
SPLICE-OPS is a list of (START END REPLACEMENT . _) covering disjoint ranges, as collected
in `create-logical-line'. used to keep virtual-item markers anchored when several folds on one
visual line each splice text out."
  (+ charpos
     (loop :for (start end replacement) :in splice-ops
           :sum (cond ((<= charpos start)
                       0)
                      ((>= charpos end)
                       (- (length replacement) (- end start)))
                      (t
                       (- start charpos))))))

(defun line-fully-invisible-p (point overlays)
  "T if an :invisible overlay spans POINT's line without either endpoint on it."
  (loop :for overlay :in overlays
        :thereis (and (overlay-get overlay :invisible)
                      (not (same-line-p (overlay-start overlay) point))
                      (not (same-line-p (overlay-end overlay) point)))))

(defun invisible-overlay-covering (point &optional (overlays (buffer-overlays (point-buffer point))))
  "Return the :invisible overlay covering POINT."
  (loop :for overlay :in overlays
        :thereis (and (overlay-get overlay :invisible)
                      (point<= (overlay-start overlay) point)
                      (point< point (overlay-end overlay))
                      overlay)))

(defun move-point-out-of-overlay (point overlay direction)
  "Move POINT to the nearest edge of OVERLAY in DIRECTION (:forward or :backward), so it does not
rest inside. usable directly as a :cursor-enter-functions handler; `fold-region' installs it by
default so a cursor never appears stuck on the hidden text of a fold."
  (if (eq direction :backward)
      (progn
        (move-point point (overlay-start overlay))
        ;; step onto the last visible position before the overlay, unless that is the buffer
        ;; start, where there is nowhere further to go.
        (character-offset point -1))
      (move-point point (overlay-end overlay))))

(defun reveal-overlay-on-cursor-enter (point overlay direction)
  (overlay-put overlay :invisible nil)
  (overlay-put overlay :show-virtual-text nil))

(defun hide-overlay-on-cursor-leave (point overlay direction)
  (overlay-put overlay :invisible t)
  (overlay-put overlay :show-virtual-text t))

(defun overlay-show-virtual-text-p (overlay)
  "Whether OVERLAY's :before-string/:after-string should render. defaults to T."
  (let ((value (getf (overlay-plist overlay) :show-virtual-text :unset)))
    (if (eq value :unset) t value)))

(defun overlay-has-cursor-hooks-p (overlay)
  (or (overlay-get overlay :cursor-enter-functions)
      (overlay-get overlay :cursor-leave-functions)))

(defun overlays-with-cursor-hooks-covering (point)
  "The overlays covering POINT that take part in cursor enter/leave tracking."
  (loop :for overlay :in (buffer-overlays (point-buffer point))
        :when (and (overlay-has-cursor-hooks-p overlay)
                   (point<= (overlay-start overlay) point)
                   (point< point (overlay-end overlay)))
          :collect overlay))

;; but reveal behavior isnt relevant for line folding
(defun place-region-placeholder-overlay (start end &key (placeholder "...") (cursor-behavior :move-out) (is-line-fold t))
  "Hide the lines of the region [START, END), leaving START's line visible with a fold marker.
returns the fold overlay. CURSOR-BEHAVIOR decides how the cursor is kept off the hidden text:
- :move-out :: move the cursor to the nearest visible edge (see `move-point-out-of-overlay').
- :reveal   :: open the fold while the cursor is inside it and close it again on leave (see
               `reveal-overlay-on-cursor-enter' / `hide-overlay-on-cursor-leave').
- nil       :: install nothing; the cursor may rest on the hidden text.
callers can also set the overlay's :cursor-enter-functions / :cursor-leave-functions directly."
  (with-point ((s start)
               (e end))
    (when is-line-fold
      (line-end s)
      ;; dont hide the newline that terminates the folded region's last line, or the line after
      ;; the fold gets merged onto the header's visual line.
      (when (start-line-p e)
        (character-offset e -1)))
    (let ((overlay (make-overlay s e 'fold-attribute)))
      (overlay-put overlay :invisible t)
      (overlay-put overlay :fold t)
      (overlay-put overlay :before-string (list placeholder 'fold-attribute))
      (ecase cursor-behavior
        (:move-out
         (overlay-put overlay :cursor-enter-functions (list 'move-point-out-of-overlay)))
        (:reveal
         (overlay-put overlay :cursor-enter-functions (list 'reveal-overlay-on-cursor-enter))
         (overlay-put overlay :cursor-leave-functions (list 'hide-overlay-on-cursor-leave)))
        ((nil)))
      overlay)))

(defun line-continuation-p (point)
  "Whether POINT's line continues a previous visual line. meaning the newline preceding it is
hidden by an :invisible overlay, so the line is not a visual line of its own.
a folded region may hide arbitrary character ranges, including the newlines that join several
buffer lines into one displayed line."
  (and (not (first-line-p point))
       (with-point ((p point))
         (line-start p)
         (character-offset p -1)
         (invisible-overlay-covering p))))

(defun collect-visual-line-string-and-attributes (vstart vend)
  (with-point ((p vstart))
    (let ((out (make-string-output-stream))
          (attributes)
          (base 0))
      (loop
        (destructuring-bind (string . attrs) (get-string-and-attributes-at-point p)
          (write-string string out)
          (loop :for (s e attr) :in attrs
                :do (push (list (+ base s) (+ base e) attr) attributes))
          (incf base (length string))
          (when (same-line-p p vend)
            (return))
          (write-char #\newline out)
          (incf base)
          (line-offset p 1)))
      (values (get-output-stream-string out)
              (nreverse attributes)))))

(defun create-logical-line (point overlays active-modes)
  "Build a logical-line for the visual line starting at POINT, joining any following buffer lines
whose preceding newline is hidden by an :invisible overlay. a single displayed line may contain
several folds that each hide arbitrary character ranges across multiple buffer lines."
  (let ((invisible-overlays
          (remove-if-not (lambda (ov) (overlay-get ov :invisible)) overlays)))
    (with-point ((vstart point)
                 (vend point))
      (line-start vstart)
      (line-end vend)
      ;; extend VEND across every newline hidden by an invisible overlay so the
      ;; visual line reaches the next *visible* newline (or the buffer end).
      (loop :until (last-line-p vend)
            :while (invisible-overlay-covering vend invisible-overlays)
            :do (line-offset vend 1)
                (line-end vend))
      (let ((overlays (remove-if-not
                       (lambda (ov)
                         (and (point<= (overlay-start ov) vend)
                              (point<= vstart (overlay-end ov))))
                       overlays)))
        (flet ((overlay-start-charpos (overlay)
                 ;; column where the overlay starts on this visual line, clamped to
                 ;; 0 when it begins before VSTART.
                 (let ((s (overlay-start overlay)))
                   (if (point<= vstart s)
                       (count-characters vstart s)
                       0)))
               (overlay-end-charpos (overlay)
                 ;; column where the overlay ends, or NIL when it extends past VEND.
                 (let ((e (overlay-end overlay)))
                   (when (point<= e vend)
                     (count-characters vstart e))))
               (start-in-line-p (overlay)
                 ;; true when the overlay's start falls within this visual line.
                 (point<= vstart (overlay-start overlay)))
               (end-in-line-p (overlay)
                 ;; true when the overlay's end falls within this visual line.
                 (point<= (overlay-end overlay) vend)))
          (let* ((end-of-line-cursor-attribute nil)
                 (extend-to-end-attribute nil)
                 (line-end-overlay nil)
                 (virtual-items)
                 (splice-ops)
                 (left-content
                   (compute-left-display-area-content active-modes
                                                      (point-buffer point)
                                                      point))
                 (tab-width (variable-value 'tab-width :default point)))
            (multiple-value-bind (string attributes)
                (collect-visual-line-string-and-attributes vstart vend)
              ;; collect string-splice operations from :invisible/:display overlays.
              (loop :for overlay :in overlays
                    :for invisible := (overlay-get overlay :invisible)
                    :for display := (overlay-get overlay :display)
                    :do (when (or invisible display)
                          (let* ((ov-start (overlay-start-charpos overlay))
                                 (ov-end (or (overlay-end-charpos overlay)
                                             (length string)))
                                 (replacement
                                   (cond
                                     (display
                                      (let ((d (alexandria:ensure-list display)))
                                        (if (stringp (first d)) (first d) "")))
                                     ((eq invisible :ellipsis) "...")
                                     (t "")))
                                 (repl-attr
                                   (when (listp display) (second display))))
                            (when (< ov-start ov-end)
                              (push (list ov-start ov-end replacement repl-attr) splice-ops)))))
              ;; apply splices right-to-left for position stability
              (when splice-ops
                (dolist (op (sort (copy-list splice-ops) #'> :key #'first))
                  (destructuring-bind (ov-start ov-end replacement repl-attr) op
                    (setf (values string attributes)
                          (splice-string string
                                         attributes
                                         ov-start
                                         ov-end
                                         replacement
                                         repl-attr)))))
              ;; process all overlays for attributes (virtual text handled separately below).
              (loop :for overlay :in overlays
                    :do (cond
                          ((typep overlay 'line-endings-overlay)
                           (when (end-in-line-p overlay)
                             (setf line-end-overlay overlay)))
                          ((typep overlay 'line-overlay)
                           (let ((attribute (overlay-attribute overlay)))
                             (setf attributes
                                   (overlay-attributes attributes
                                                       0
                                                       (length string)
                                                       attribute))
                             (setf extend-to-end-attribute attribute)))
                          ((typep overlay 'cursor-overlay)
                           ;; remap the cursor into the spliced string so it lands on the right
                           ;; column past any folds on this visual line.
                           (let* ((ov-start (adjust-charpos-for-splices
                                             (overlay-start-charpos overlay) splice-ops))
                                  (ov-end (1+ ov-start))
                                  (ov-attr (overlay-attribute overlay)))
                             (unless (cursor-overlay-fake-p overlay)
                               (set-cursor-attribute ov-attr))
                             (if (<= (length string) ov-start)
                                 (setf end-of-line-cursor-attribute ov-attr)
                                 (setf attributes
                                       (overlay-attributes attributes ov-start ov-end ov-attr)))))
                          (t
                           (let ((ov-start (adjust-charpos-for-splices
                                            (overlay-start-charpos overlay) splice-ops))
                                 (ov-end (let ((e (overlay-end-charpos overlay)))
                                           (when e
                                             (adjust-charpos-for-splices e splice-ops))))
                                 (ov-attr (overlay-attribute overlay))
                                 (invisible (overlay-get overlay :invisible))
                                 (display (overlay-get overlay :display)))
                             ;; plain attribute (only when not replaced by invisible/display)
                             (when (and ov-attr (not invisible) (not display))
                               (unless ov-end
                                 (setf extend-to-end-attribute ov-attr))
                               (setf attributes
                                     (overlay-attributes attributes
                                                         ov-start
                                                         (or ov-end (length string))
                                                         ov-attr)))))))
              ;; virtual text from :before-string/:after-string overlays. emit each overlay's
              ;; :before then :after, visiting overlays in (end, start) order: at any shared
              ;; charpos an overlay closing there (smaller end) is emitted before one opening
              ;; there, so trailing :after-strings precede leading :before-strings, but a
              ;; zero-length overlay's own pair stays adjacent. a final stable sort by charpos
              ;; groups them without affecting this order.
              (loop :for overlay :in (stable-sort
                                      (loop :for overlay :in overlays
                                            :when (and (overlay-show-virtual-text-p overlay)
                                                       (or (overlay-get overlay :before-string)
                                                           (overlay-get overlay :after-string)))
                                              :collect overlay)
                                      (lambda (a b)
                                        (let ((a-end (or (overlay-end-charpos a) (length string)))
                                              (b-end (or (overlay-end-charpos b) (length string))))
                                          (if (= a-end b-end)
                                              (< (overlay-start-charpos a)
                                                 (overlay-start-charpos b))
                                              (< a-end b-end)))))
                    :for before-str := (overlay-get overlay :before-string)
                    :for after-str := (overlay-get overlay :after-string)
                    :do (when (and before-str (start-in-line-p overlay))
                          (let ((bs (alexandria:ensure-list before-str)))
                            (push (make-virtual-item :charpos (overlay-start-charpos overlay)
                                                     :string (first bs)
                                                     :attribute (second bs))
                                  virtual-items)))
                        (when (and after-str (end-in-line-p overlay))
                          (let ((as (alexandria:ensure-list after-str)))
                            (push (make-virtual-item :charpos (or (overlay-end-charpos overlay)
                                                                  (length string))
                                                     :string (first as)
                                                     :attribute (second as))
                                  virtual-items))))
              ;; markers were positioned in raw coordinates; remap them into the
              ;; spliced string so several folds on one visual line stay anchored.
              (dolist (vi virtual-items)
                (setf (virtual-item-charpos vi)
                      (adjust-charpos-for-splices (virtual-item-charpos vi) splice-ops)))
              (setf virtual-items
                    (stable-sort (nreverse virtual-items) #'< :key #'virtual-item-charpos))
              (setf (values string attributes) (expand-tab string attributes tab-width))
              (let ((charpos (point-charpos point)))
                (when (< 0 charpos)
                  (psetf string (subseq string charpos)
                         attributes (lem/buffer/line:subseq-elements
                                     attributes charpos (length string)))
                  ;; adjust virtual-item positions for the charpos clip (order preserved)
                  (setf virtual-items
                        (loop :for vi :in virtual-items
                              :when (>= (virtual-item-charpos vi) charpos)
                                :collect (make-virtual-item
                                          :charpos (- (virtual-item-charpos vi) charpos)
                                          :string (virtual-item-string vi)
                                          :attribute (virtual-item-attribute vi))))))
              (make-logical-line
               :string string
               :attributes attributes
               :virtual-items virtual-items
               :left-content left-content
               :extend-to-end extend-to-end-attribute
               :end-of-line-cursor-attribute end-of-line-cursor-attribute
               :line-end-overlay line-end-overlay))))))))

(defstruct string-with-attribute-item
  string
  attribute)

(defstruct cursor-item
  attribute
  string)

(defstruct eol-cursor-item
  attribute
  true-cursor-p)

(defstruct extend-to-eol-item
  color)

(defstruct line-end-item
  text
  attribute
  offset)

(defmethod item-string ((item string-with-attribute-item))
  (string-with-attribute-item-string item))

(defmethod item-string ((item cursor-item))
  (cursor-item-string item))

(defmethod item-string ((item eol-cursor-item))
  " ")

(defmethod item-string ((item extend-to-eol-item))
  "")

(defmethod item-attribute ((item string-with-attribute-item))
  (string-with-attribute-item-attribute item))

(defmethod item-attribute ((item cursor-item))
  (cursor-item-attribute item))

(defmethod item-attribute ((item eol-cursor-item))
  (eol-cursor-item-attribute item))

(defmethod item-attribute ((item extend-to-eol-item))
  nil)

(defun add-or-merge-item (item items)
  "add ITEM to the front of ITEMS, or merge it into the previous
string-with-attribute-item when both carry the same attribute. returns the updated list."
  (let ((last-item (first items)))
    (if (and (string-with-attribute-item-p last-item)
             (string-with-attribute-item-p item)
             (equal (string-with-attribute-item-attribute last-item)
                    (string-with-attribute-item-attribute item)))
        (progn
          (setf (string-with-attribute-item-string last-item)
                (str:concat (string-with-attribute-item-string last-item)
                            (string-with-attribute-item-string item)))
          items)
        (cons item items))))

(defun compute-items-from-string-and-attributes (string attributes)
  (handler-case
      (let ((items '()))
        (loop :for last-pos := 0 :then end
              :for (start end attribute) :in attributes
              :do (unless (= last-pos start)
                    (setf items (add-or-merge-item
                                 (make-string-with-attribute-item :string (subseq string last-pos start))
                                 items)))
                  (setf items (add-or-merge-item
                               (if (cursor-attribute-p attribute)
                                   (make-cursor-item :string (subseq string start end) :attribute attribute)
                                   (make-string-with-attribute-item
                                    :string (subseq string start end)
                                    :attribute attribute))
                               items))
              :finally (push (make-string-with-attribute-item :string (subseq string last-pos))
                             items))
        items)
    (error (e)
      (log:error e string attributes)
      nil)))

(defun inject-virtual-items (string attributes virtual-items)
  "produce items from STRING and ATTRIBUTES, injecting VIRTUAL-ITEMS at their charposes.
VIRTUAL-ITEMS arrive in draw order (from `create-logical-line')."
  (let* (;; all positions where we may need to split: attribute boundaries, virtual charposes.
         (positions
           (sort (remove-duplicates
                  (nconc (list 0 (length string))
                         (mapcar #'virtual-item-charpos virtual-items)
                         (mapcan (lambda (span) (list (first span) (second span)))
                                 attributes)))
                 #'<))
         ;; VIRTUAL-ITEMS are already sorted by charpos in draw order
         (pending virtual-items)
         (items))
    (flet ((add-virtuals-at (pos)
             (loop :while (and pending (= (virtual-item-charpos (first pending)) pos))
                   :do (let ((vi (pop pending)))
                         (setf items (add-or-merge-item
                                      (make-string-with-attribute-item
                                       :string (virtual-item-string vi)
                                       :attribute (virtual-item-attribute vi))
                                      items))))))
      ;; walk segments between break positions, injecting virtual items at each boundary
      (loop :for (pos . rest) :on positions
            :while rest
            :for next-pos := (first rest)
            :do (add-virtuals-at pos)
                (unless (= pos next-pos)
                  (let* ((seg (subseq string pos next-pos))
                         (attr (loop :for (start end attribute) :in attributes
                                     :when (and (<= start pos) (>= end next-pos))
                                       :return attribute)))
                    (unless (string= seg "")
                      (setf items (add-or-merge-item
                                   (if (cursor-attribute-p attr)
                                       (make-cursor-item :string seg :attribute attr)
                                       (make-string-with-attribute-item :string seg
                                                                        :attribute attr))
                                   items))))))
      ;; virtual items at the very end of the string
      (add-virtuals-at (length string)))
    items))

(defun compute-items-from-logical-line (logical-line)
  (let ((items
          (if (logical-line-virtual-items logical-line)
              (inject-virtual-items (logical-line-string logical-line)
                                    (logical-line-attributes logical-line)
                                    (logical-line-virtual-items logical-line))
              (compute-items-from-string-and-attributes (logical-line-string logical-line)
                                                        (logical-line-attributes logical-line)))))
    (alexandria:when-let (attribute
                          (logical-line-extend-to-end logical-line))
      (push (make-extend-to-eol-item :color (attribute-background-color attribute))
            items))
    (alexandria:when-let (attribute
                          (logical-line-end-of-line-cursor-attribute logical-line))
      (push (make-eol-cursor-item :attribute attribute
                                  :true-cursor-p (cursor-attribute-p attribute))
            items))
    (values (nreverse items)
            (alexandria:when-let (overlay
                                  (logical-line-line-end-overlay logical-line))
              (make-line-end-item :text (line-endings-overlay-text overlay)
                                  :attribute (overlay-attribute overlay)
                                  :offset (line-endings-overlay-offset overlay))))))

(defun make-temporary-highlight-line-overlay (buffer)
  (when (and (variable-value 'highlight-line :default (current-buffer))
             (current-theme))
    (alexandria:when-let ((color (highlight-line-color)))
      (make-line-overlay (buffer-point buffer)
                         (make-attribute :background color)
                         :temporary t))))

(defgeneric make-region-overlays-using-global-mode (global-mode cursor))

(defmethod make-region-overlays-using-global-mode ((global-mode emacs-mode) cursor)
  (let ((mark (cursor-mark cursor)))
    (when (mark-active-p mark)
      (list (make-overlay cursor
                    (mark-point mark)
                    'region
                    :temporary t)))))

(defun make-cursor-overlay* (point)
  (make-cursor-overlay
   point
   (if (typep point 'fake-cursor)
       'fake-cursor
       'cursor)
   :fake (typep point 'fake-cursor)))

(defun get-window-overlays (window)
  (let* ((buffer (window-buffer window))
         (overlays (buffer-overlays buffer)))
    (when (eq (current-window) window)
      (dolist (cursor (buffer-cursors buffer))
        (alexandria:when-let ((region-overlays (make-region-overlays-using-global-mode (current-global-mode) cursor)))
          (dolist (ol region-overlays) (push ol overlays))))
      (if-push (make-temporary-highlight-line-overlay buffer)
               overlays))
    (if (and (eq window (current-window))
             (not (window-cursor-invisible-p window))
             (not (lem-if:cursor-blink-off-p (implementation))))
        (append overlays
                (mapcar #'make-cursor-overlay*
                        (buffer-cursors (window-buffer window))))
        overlays)))

(defun call-do-logical-line (window function)
  (with-point ((point (window-view-point window)))
    (let* ((overlays (get-window-overlays window))
           (active-modes (get-active-modes-class-instance (window-buffer window)))
           (*active-modes* active-modes))
      (loop :for logical-line := (create-logical-line point overlays active-modes)
            :do (when logical-line
                  (funcall function logical-line))
                (loop
                  (unless (line-offset point 1)
                    (return-from call-do-logical-line))
                  (unless (line-continuation-p point)
                    (return)))))))

(defmacro do-logical-line ((logical-line window) &body body)
  `(call-do-logical-line ,window (lambda (,logical-line) ,@body)))
