(in-package :lem-core)

(defvar *line-wrap* nil)

(defun window-view-width (window)
  (lem-if:view-width (implementation) (window-view window)))

(defun window-view-height (window)
  (lem-if:view-height (implementation) (window-view window)))

(defun drawing-cache (window)
  (window-parameter window 'redrawing-cache))

(defun (setf drawing-cache) (value window)
  (setf (window-parameter window 'redrawing-cache) value))

(defclass drawing-object ()
  ((width :initform nil :accessor drawing-object-width)))

(defclass void-object (drawing-object) ())

(defclass text-object (drawing-object)
  ((surface :initarg :surface :initform nil :accessor text-object-surface)
   (string :initarg :string :reader text-object-string)
   (attribute :initarg :attribute :reader text-object-attribute)
   (type :initarg :type :reader text-object-type)
   (within-cursor :initform nil :initarg :within-cursor :reader text-object-within-cursor-p)))

(defmethod print-object ((drawing-object text-object) stream)
  (print-unreadable-object (drawing-object stream :type t)
    (format stream
            "~S ~S"
            (text-object-string drawing-object)
            (text-object-attribute drawing-object))))

(defclass control-character-object (text-object) ())

(defclass icon-object (text-object) ())
(defclass folder-object (text-object) ())
(defclass emoji-object (text-object) ())

(defclass eol-cursor-object (drawing-object)
  ((color :initarg :color
          :reader eol-cursor-object-color)
   (attribute :initarg :attribute
              :reader eol-cursor-object-attribute)
   (true-cursor-p :initarg :true-cursor-p
                  :initform nil
                  :reader eol-cursor-object-true-cursor-p)))

(defclass extend-to-eol-object (drawing-object)
  ((color :initarg :color
          :reader extend-to-eol-object-color)))

(defclass line-end-object (text-object)
  ((offset :initarg :offset
           :reader line-end-object-offset)))

(defclass image-object (drawing-object)
  ((image :initarg :image :reader image-object-image)
   ;; (not WIDTH: that is the slot of DRAWING-OBJECT that caches the width
   ;; of the object for the frontend, which may count cells, not pixels)
   (image-width :initarg :width :reader image-object-width)
   (height :initarg :height :reader image-object-height)
   (attribute :initarg :attribute :reader image-object-attribute)))

(defmethod cursor-object-p (drawing-object)
  nil)

(defmethod cursor-object-p ((drawing-object text-object))
  (text-object-within-cursor-p drawing-object))

(defmethod cursor-object-p ((drawing-object eol-cursor-object))
  t)

(defgeneric drawing-object-equal (drawing-object-1 drawing-object-2))

(defmethod drawing-object-equal (drawing-object-1 drawing-object-2)
  nil)

(defmethod drawing-object-equal ((drawing-object-1 void-object) (drawing-object-2 void-object))
  t)

(defmethod drawing-object-equal ((drawing-object-1 text-object) (drawing-object-2 text-object))
  (and (equal (text-object-string drawing-object-1)
              (text-object-string drawing-object-2))
       (attribute-equal (text-object-attribute drawing-object-1)
                        (text-object-attribute drawing-object-2))
       (eq (text-object-type drawing-object-1)
           (text-object-type drawing-object-2))
       (eq (text-object-within-cursor-p drawing-object-1)
           (text-object-within-cursor-p drawing-object-2))))

(defmethod drawing-object-equal ((drawing-object-1 eol-cursor-object) (drawing-object-2 eol-cursor-object))
  (equal (eol-cursor-object-color drawing-object-1)
         (eol-cursor-object-color drawing-object-2)))

(defmethod drawing-object-equal ((drawing-object-1 extend-to-eol-object) (drawing-object-2 extend-to-eol-object))
  (equal (extend-to-eol-object-color drawing-object-1)
         (extend-to-eol-object-color drawing-object-2)))

(defmethod drawing-object-equal ((drawing-object-1 line-end-object) (drawing-object-2 line-end-object))
  (and (call-next-method)
       (equal (line-end-object-offset drawing-object-1)
              (line-end-object-offset drawing-object-2))))

(defmethod drawing-object-equal ((drawing-object-1 image-object) (drawing-object-2 image-object))
  (and (eq (image-object-image drawing-object-1) (image-object-image drawing-object-1))
       (equal (image-object-width drawing-object-1) (image-object-width drawing-object-1))
       (equal (image-object-height drawing-object-1) (image-object-height drawing-object-1))))


(defgeneric drawing-object-mergable-p (drawing-object-1 drawing-object-2))

(defmethod drawing-object-mergable-p (drawing-object-1 drawing-object-2)
  nil)

(defmethod drawing-object-mergable-p ((drawing-object-1 void-object) (drawing-object-2 void-object))
  t)

(defmethod drawing-object-mergable-p ((drawing-object-1 text-object) (drawing-object-2 text-object))
  (and (attribute-equal (text-object-attribute drawing-object-1)
                        (text-object-attribute drawing-object-2))
       (eq (text-object-type drawing-object-1)
           (text-object-type drawing-object-2))
       (eq (text-object-within-cursor-p drawing-object-1)
           (text-object-within-cursor-p drawing-object-2))))

(defmethod drawing-object-mergable-p ((drawing-object-1 eol-cursor-object) (drawing-object-2 eol-cursor-object))
  (equal (eol-cursor-object-color drawing-object-1)
         (eol-cursor-object-color drawing-object-2)))

(defmethod drawing-object-mergable-p ((drawing-object-1 extend-to-eol-object) (drawing-object-2 extend-to-eol-object))
  (equal (extend-to-eol-object-color drawing-object-1)
         (extend-to-eol-object-color drawing-object-2)))

(defmethod drawing-object-mergable-p ((drawing-object-1 line-end-object) (drawing-object-2 line-end-object))
  (and (call-next-method)
       (equal (line-end-object-offset drawing-object-1)
              (line-end-object-offset drawing-object-2))))

(defmethod drawing-object-mergable-p ((drawing-object-1 image-object) (drawing-object-2 image-object))
  (and (eq (image-object-image drawing-object-1) (image-object-image drawing-object-1))
       (equal (image-object-width drawing-object-1) (image-object-width drawing-object-1))
       (equal (image-object-height drawing-object-1) (image-object-height drawing-object-1))))


(defgeneric drawing-object-merge (drawing-object-1 drawing-object-2))

(defmethod drawing-object-merge ((drawing-object-1 void-object) (drawing-object-2 void-object))
  drawing-object-1)

(defmethod drawing-object-merge ((drawing-object-1 text-object) (drawing-object-2 text-object))
  ;; Destructive merge: mutate drawing-object-1 in place to avoid allocating
  ;; a new CLOS instance.  Safe because reduce-list discards the originals.
  (setf (slot-value drawing-object-1 'string)
        (str:concat (text-object-string drawing-object-1)
                    (text-object-string drawing-object-2)))
  ;; Reset cached width and surface since string changed.  The surface is the
  ;; frontend-rendered glyph bitmap; leaving the pre-merge surface in place
  ;; makes draw-time render only the original (shorter) string and drop the
  ;; rest of the merged run.
  (setf (drawing-object-width drawing-object-1) nil)
  (setf (text-object-surface drawing-object-1) nil)
  drawing-object-1)

(defmethod drawing-object-merge ((drawing-object-1 eol-cursor-object) (drawing-object-2 eol-cursor-object))
  drawing-object-1)

(defmethod drawing-object-merge ((drawing-object-1 extend-to-eol-object) (drawing-object-2 extend-to-eol-object))
  drawing-object-1)

(defmethod drawing-object-merge ((drawing-object-1 line-end-object) (drawing-object-2 line-end-object))
  ;; Destructive merge: mutate in place like text-object
  (setf (slot-value drawing-object-1 'string)
        (str:concat (text-object-string drawing-object-1)
                    (text-object-string drawing-object-2)))
  (setf (drawing-object-width drawing-object-1) nil)
  (setf (text-object-surface drawing-object-1) nil)
  drawing-object-1)

(defmethod drawing-object-merge ((drawing-object-1 image-object) (drawing-object-2 image-object))
  drawing-object-1)



(defun object-width (drawing-object)
  (or (drawing-object-width drawing-object)
      (setf (drawing-object-width drawing-object)
            (lem-if:object-width (implementation) drawing-object))))

(defun object-height (drawing-object)
  (lem-if:object-height (implementation) drawing-object))

(defun split-string-by-character-type (string)
  (loop :with pos := 0 :and items := '()
        :while (< pos (length string))
        :for type := (char-type (char string pos))
        :do (loop :with start := pos
                  :do (incf pos)
                  :while (and (< pos (length string))
                              (eq type (char-type (char string pos)))
                              (not (eq type :control)))
                  :finally (push (cons type (subseq string start pos)) items))
        :finally (return (nreverse items))))

(defun make-line-end-object (string attribute type offset)
  (let ((attribute (and attribute (ensure-attribute attribute nil))))
    (make-instance 'line-end-object
                   :offset offset
                   :string string
                   :attribute attribute
                   :type type)))

;;; Split make-instance calls by class name so SBCL can cache each constructor
;;; independently (compile-time-known class name → inlined CTOR, bypassing
;;; the generic ENSURE-CACHED-CTOR lookup on every call).
(defun make-object-with-type (string attribute type)
  (let* ((attribute (and attribute (ensure-attribute attribute nil)))
         (within-cursor (and attribute (cursor-attribute-p attribute)))
         (resolved-string (case type
                            (:control (control-char (char string 0)))
                            (:zero-width
                             (make-string (length string) :initial-element #\·))
                            (otherwise string)))
         (resolved-attribute (case type
                               ((:control :zero-width)
                                (let ((attr (ensure-attribute 'special-char-attribute nil)))
                                  (if attribute
                                      (merge-attribute attribute attr)
                                      attr)))
                               (otherwise attribute))))
    (case type
      (:folder
       (make-instance 'folder-object
                      :string resolved-string :attribute resolved-attribute
                      :type type :within-cursor within-cursor))
      (:icon
       (make-instance 'icon-object
                      :string resolved-string :attribute resolved-attribute
                      :type type :within-cursor within-cursor))
      (:emoji
       (make-instance 'emoji-object
                      :string resolved-string :attribute resolved-attribute
                      :type type :within-cursor within-cursor))
      (:control
       (make-instance 'control-character-object
                      :string resolved-string :attribute resolved-attribute
                      :type type :within-cursor within-cursor))
      (otherwise
       (make-instance 'text-object
                      :string resolved-string :attribute resolved-attribute
                      :type type :within-cursor within-cursor)))))

(defun create-drawing-object (item)
  (cond ((and *line-wrap* (typep item 'eol-cursor-item))
         (list (make-instance 'eol-cursor-object
                              :attribute (eol-cursor-item-attribute item)
                              :color (parse-color
                                      (attribute-background
                                       (eol-cursor-item-attribute item)))
                              :true-cursor-p (eol-cursor-item-true-cursor-p item))))
        ((typep item 'extend-to-eol-item)
         (list (make-instance 'extend-to-eol-object :color (extend-to-eol-item-color item))))
        ((typep item 'line-end-item)
         (let ((string (line-end-item-text item))
               (attribute (line-end-item-attribute item)))
           (loop :for (type . string) :in (split-string-by-character-type string)
                 :unless (alexandria:emptyp string)
                 :collect (make-line-end-object string
                                                attribute
                                                type
                                                (line-end-item-offset item)))))
        (t
         (let ((string (item-string item))
               (attribute (item-attribute item)))
           (cond ((alexandria:emptyp string)
                  (list (make-instance 'void-object)))
                 ((and attribute (attribute-image attribute))
                  (list (make-instance 'image-object
                                       :image (attribute-image attribute)
                                       :width (attribute-width attribute)
                                       :height (attribute-height attribute)
                                       :attribute attribute)))
                 (t
                  (loop :for (type . string) :in (split-string-by-character-type string)
                        :unless (alexandria:emptyp string)
                        :collect (make-object-with-type string attribute type))))))))

(defun create-drawing-objects (logical-line)
  (multiple-value-bind (items line-end-item)
      (compute-items-from-logical-line logical-line)
    (append (loop :for item :in items
                  :append (create-drawing-object item))
            (when line-end-item
              (create-drawing-object line-end-item)))))

(defun make-letter-object (character attribute)
  (make-object-with-type (string character)
                         attribute
                         (char-type character)))

(defun separate-objects-by-width (objects view-width buffer)
  (flet ((explode-object (text-object)
           (check-type text-object text-object)
           (let* ((string (text-object-string text-object))
                  (char-type (char-type (char string 0)))
                  (n (floor (length string) 2)))
             (loop :for part-string :in (list (subseq string 0 n)
                                              (subseq string n))
                   :unless (alexandria:emptyp part-string)
                   :collect (make-object-with-type
                             part-string
                             (text-object-attribute text-object) char-type)))))
    (let ((wrap-line-character (variable-value 'wrap-line-character :default buffer))
          (wrap-line-attribute (variable-value 'wrap-line-attribute :default buffer)))
      (loop :with total-width := 0
            :and physical-line-objects := '()
            :for object := (pop objects)
            :while object
            :do (cond ((and (typep object 'text-object)
                            (<= view-width (+ total-width (object-width object))))
                       (cond ((< 1 (length (text-object-string object)))
                              (setf objects (nconc (explode-object object) objects)))
                             (t
                              (push object objects)
                              (push (make-letter-object wrap-line-character
                                                        wrap-line-attribute)
                                    physical-line-objects)
                              (return (values (nreverse physical-line-objects) objects)))))
                      (t
                       (incf total-width (object-width object))
                       (push object physical-line-objects)))
            :finally (return (nreverse physical-line-objects))))))

(defun render-line (view x y objects height)
  (lem-if:render-line (implementation) view x y objects height))

(defun reduce-list (list
                    &key (test (alexandria:required-argument :test))
                         (merge (alexandria:required-argument :merge)))
  ;; Destructive: operates on LIST in place.  Callers must pass freshly-
  ;; allocated lists (clip-objects-to-display-range and append both do).
  (let ((new '()))
    (loop :for current-list := list
          :for (current next rest) := current-list
          :do (cond ((alexandria:length= current-list 0)
                     (return))
                    ((alexandria:length= current-list 1)
                     (push current new)
                     (return))
                    ((funcall test current next)
                     (setf (car current-list)
                           (funcall merge current next))
                     (setf (cdr current-list)
                           (cddr current-list)))
                    (t
                     (push current new)
                     (pop list))))
    (nreverse new)))

(defun reduce-objects (objects)
  (reduce-list objects
               :test #'drawing-object-mergable-p
               :merge #'drawing-object-merge))

(defun drawing-objects-equal (objects1 objects2)
  "Compare two lists of drawing objects for equality.
Assumes inputs are already reduced (no adjacent mergeable objects)."
  (when (alexandria:length= objects1 objects2)
    (loop :for obj1 :in objects1
          :for obj2 :in objects2
          :always (drawing-object-equal obj1 obj2))))

(defun validate-cache-p (window y height objects)
  (loop :for (cache-y cache-height cache-objects) :in (drawing-cache window)
        :when (and (= y cache-y)
                   (= height cache-height)
                   (drawing-objects-equal objects cache-objects))
        :return t))

(defun invalidate-cache (window y height)
  "Drop the cache entries of the rows that a line drawn at Y, HEIGHT high,
overlaps: a taller line covers the entries of the rows under it, which no
longer hold what those entries say (a later line equal to one of them
would otherwise not be drawn)."
  (setf (drawing-cache window)
        (remove-if (lambda (elt)
                     (destructuring-bind (cache-y cache-height drawing-objects) elt
                       (declare (ignore drawing-objects))
                       (and (< cache-y (+ y height))
                            (< y (+ cache-y cache-height)))))
                   (drawing-cache window))))

(defun remove-drawing-cache-entries-from (entries y)
  "Return ENTRIES with drawing-cache rows at or below Y removed.
Pure helper over the entry list so the eviction can be unit tested without
a window."
  (remove-if (lambda (elt)
               (>= (first elt) y))
             entries))

(defun invalidate-drawing-cache-from (window y)
  "Drop drawing-cache entries for screen rows at or below Y.
Counterpart to CLEAR-LINE-FINGERPRINT-CACHE-FROM for the drawing-object
cache: when the area from Y down is blanked by CLEAR-TO-END-OF-WINDOW
those rows no longer hold the objects their cache entries describe.  A
later frame whose restored content matches a stale entry (e.g. undoing a
large deletion) would otherwise pass VALIDATE-CACHE-P and skip the render,
leaving the row blank on persistent-texture frontends such as SDL2."
  (setf (drawing-cache window)
        (remove-drawing-cache-entries-from (drawing-cache window) y)))

(defun update-and-validate-cache-p (window y height objects)
  "Check cache validity, reducing objects once before storing.
Returns T if the cached entry matches (render can be skipped)."
  (let ((reduced (reduce-objects objects)))
    (cond ((validate-cache-p window y height reduced) t)
          (t
           (invalidate-cache window y height)
           (push (list y height reduced)
                 (drawing-cache window))
           nil))))

(defun render-line-with-caching (window x y objects height)
  (unless (update-and-validate-cache-p window y height objects)
    (render-line (window-view window) x y objects height)))

(defun max-height-of-objects (objects)
  (loop :for object :in objects
        :maximize (object-height object)))

;;; Line fingerprint cache — avoids creating drawing objects for unchanged lines

(defun line-fingerprint-cache (window)
  "Return WINDOW's line-fingerprint hash table, lazily allocating one.
Stored on the window's parameter plist so the cache lives and dies with
the window itself (no global state)."
  (or (window-parameter window 'line-fingerprint-cache)
      (setf (window-parameter window 'line-fingerprint-cache)
            (make-hash-table :test 'eql))))

(defun clear-line-fingerprint-cache (window)
  "Drop all cached line fingerprints on WINDOW.  Invoked when the screen
is force-redrawn or marked as needing redraw, since cached heights may
no longer reflect the current layout."
  (alexandria:when-let ((cache (window-parameter window 'line-fingerprint-cache)))
    (clrhash cache)))

(defun evict-line-fingerprints-from (cache y)
  "Remove fingerprint entries in CACHE for screen rows at or below Y.
Pure helper over the hash table so it can be unit tested without a window."
  (loop :for key :being :the :hash-keys :of cache
        :when (>= key y)
        :collect key :into stale
        :finally (dolist (key stale)
                   (remhash key cache))))

(defun clear-line-fingerprint-cache-from (window y)
  "Drop cached line fingerprints for screen rows at or below Y on WINDOW.
Called when the area from Y down is about to be blanked by
CLEAR-TO-END-OF-WINDOW: those rows no longer hold the content their cached
fingerprints describe.  Leaving them would let a later frame whose restored
content happens to match a stale fingerprint (e.g. undoing a large
deletion) skip the render, leaving the row blank on persistent-texture
frontends such as SDL2."
  (alexandria:when-let ((cache (window-parameter window 'line-fingerprint-cache)))
    (evict-line-fingerprints-from cache y)))

(defun item-content-hash (item)
  "Return a content-based hash for ITEM.

SXHASH on STANDARD-OBJECTs and STRUCTURE-OBJECTs is identity-based in
SBCL, so an attribute mutated in place (e.g. recoloring the shared CURSOR
attribute via SET-ATTRIBUTE) keeps the same SXHASH even though its visible
content changed.  Hashing attribute/color content keeps the line
fingerprint consistent with ATTRIBUTE-EQUAL and avoids stale glyphs
(ghosting) when an attribute is mutated rather than replaced."
  (typecase item
    (attribute
     (let ((hash 5381))
       (declare (type fixnum hash))
       (flet ((mix (x)
                (setf hash (logand most-positive-fixnum
                                   (+ (* hash 33) (item-content-hash x))))))
         (mix (attribute-foreground item))
         (mix (attribute-background item))
         (mix (attribute-reverse item))
         (mix (attribute-bold item))
         (mix (attribute-underline item))
         (mix (attribute-underline-style item))
         (mix (attribute-italic item))
         (mix (attribute-strikethrough item))
         (mix (attribute-dim item))
         (mix (attribute-link item)))
       hash))
    (lem/common/color:color
     (logand most-positive-fixnum
             (+ (* 33 (+ (* 33 (lem/common/color:color-red item))
                         (lem/common/color:color-green item)))
                (lem/common/color:color-blue item))))
    (cons
     ;; Descend so attributes nested inside sublists (the (start end
     ;; attribute) entries of LOGICAL-LINE-ATTRIBUTES) are content-hashed
     ;; rather than caught by the identity-based SXHASH of the sublist.
     (let ((hash 5381))
       (declare (type fixnum hash))
       (loop :for x := item :then (cdr x)
             :while (consp x)
             :do (setf hash (logand most-positive-fixnum
                                    (+ (* hash 33) (item-content-hash (car x)))))
             :finally (when x
                        (setf hash (logand most-positive-fixnum
                                           (+ (* hash 33) (item-content-hash x))))))
       hash))
    (t (sxhash item))))

(defun djb2 (hash item)
  "Hash with seed and item using djb2 hash algorithm"
  (declare (type fixnum hash))
  (logand most-positive-fixnum
          (+ (* hash 33)
             (item-content-hash item))))

(defun mix-hashes (&rest items)
  "Fold ITEMS into one fixnum hash, descending into nested lists. Iterative
over the top-level spine and tolerant of improper (dotted) lists."
  (declare (dynamic-extent items))
  (let ((hash 5381))
    (labels ((mix-list (x)
               (loop :while (consp x)
                     :do (setf hash (djb2 hash (car x)))
                         (setf x (cdr x)))
               (when x
                 (setf hash (djb2 hash x)))))
      (dolist (item items)
        (if (consp item)
            (mix-list item)
            (setf hash (djb2 hash item))))
      hash)))

(defun compute-line-fingerprint (logical-line scroll-start left-side-width)
  "Compute a cheap fingerprint for a logical line's display state."
  (mix-hashes
   (logical-line-string logical-line)
   (logical-line-attributes logical-line)
   (logical-line-end-of-line-cursor-attribute logical-line)
   (logical-line-extend-to-end logical-line)
   (logical-line-line-end-overlay logical-line)
   (logical-line-virtual-items logical-line)
   scroll-start
   left-side-width))

(defun check-line-fingerprint (window y fingerprint)
  "Check if the fingerprint for line at Y matches. Returns cached height or NIL."
  (let ((cache (line-fingerprint-cache window)))
    (multiple-value-bind (entry found) (gethash y cache)
      (when (and found (eql (car entry) fingerprint))
        (cdr entry)))))

(defun update-line-fingerprint (window y fingerprint height)
  "Store the fingerprint and height for line at Y."
  (setf (gethash y (line-fingerprint-cache window))
        (cons fingerprint height)))

(defun redraw-logical-line-when-line-wrapping (window
                                               y
                                               logical-line
                                               left-side-objects
                                               left-side-width)
  (let* ((left-side-characters (loop :for obj :in left-side-objects
                                     :when (typep obj 'text-object)
                                     :sum (length (text-object-string obj)))))
    (multiple-value-bind (first-line-objects rest-line-objects)
        (separate-objects-by-width (create-drawing-objects logical-line)
                                   (- (window-view-width window) left-side-width)
                                   (window-buffer window))
      (let ((wrapped-left-side-objects
              (when rest-line-objects
                (copy-list (compute-wrap-left-area-content
                            *active-modes*
                            left-side-width
                            left-side-characters)))))
        (let ((total-height 0)
              (objects first-line-objects))
          (loop
            (unless objects (return))
            (let* ((all-objects (append left-side-objects objects))
                   (height (max-height-of-objects all-objects)))
              (render-line-with-caching window 0 y all-objects height)
              (incf y height)
              (setq left-side-objects wrapped-left-side-objects)
              (incf total-height height)
              (unless (< y (window-height window))
                (return)))
            (setf (values objects rest-line-objects)
                  (separate-objects-by-width rest-line-objects
                                             (- (window-view-width window) left-side-width)
                                             (window-buffer window))))
          total-height)))))

(defun find-cursor-object (objects)
  (loop :for object :in objects
        :and x := 0 :then (+ x (object-width object))
        :when (cursor-object-p object)
        :return (values object x)))

(defun horizontal-scroll-start (window)
  (or (window-parameter window 'horizontal-scroll-start)
      0))

(defun (setf horizontal-scroll-start) (x window)
  (setf (window-parameter window 'horizontal-scroll-start) x))

(defun extract-object-in-display-range (objects start-x end-x)
  (loop :for object :in objects
        :and x := 0 :then (+ x (object-width object))
        :when (and (<= start-x x)
                   (<= (+ x (object-width object)) end-x))
        :collect object))

(defun clip-objects-to-display-range (objects start-x end-x)
  "Extract and clip objects to [start-x, end-x). Only explodes text-objects
that straddle a boundary; fully-visible objects pass through unchanged.
For straddling text-objects, computes per-character width from the object's
total width / length (exact for monospace fonts) to find the visible substring,
creating zero temporary letter-objects."
  (let ((result '())
        (x 0))
    (dolist (object objects)
      (let* ((w (object-width object))
             (obj-end (+ x w)))
        (cond
          ;; Fully before visible range - skip
          ((<= obj-end start-x) nil)
          ;; Fully after visible range - done
          ((<= end-x x) (return))
          ;; Fully within visible range - include as-is (no allocation)
          ((and (<= start-x x) (<= obj-end end-x))
           (push object result))
          ;; Straddles boundary and is a text-object - extract visible substring
          ;; Uses total-width/length to compute per-char width (exact for monospace,
          ;; the font type Lem uses). No letter-object creation needed.
          ((typep object 'text-object)
           (let* ((string (text-object-string object))
                  (len (length string))
                  (per-char-width (if (> len 0) (/ w len) 0))
                  (char-x x)
                  (start-idx nil)
                  (end-idx 0))
             (loop :for i :from 0 :below len
                   :do (cond
                         ((>= char-x end-x) (return))
                         ((and (<= start-x char-x)
                               (<= (+ char-x per-char-width) end-x))
                          (when (null start-idx) (setf start-idx i))
                          (setf end-idx (1+ i))))
                       (incf char-x per-char-width))
             ;; Create one text-object for the visible substring
             (when start-idx
               (push (make-object-with-type
                      (subseq string start-idx end-idx)
                      (text-object-attribute object)
                      (text-object-type object))
                     result))))
          ;; Non-text objects straddling boundary - include
          (t (push object result)))
        (incf x w)))
    (nreverse result)))

(defun redraw-logical-line-when-horizontal-scroll (window
                                                   y
                                                   logical-line
                                                   left-side-objects
                                                   left-side-width)
  (let* ((scroll-before (horizontal-scroll-start window))
         (fingerprint (compute-line-fingerprint logical-line
                                                scroll-before
                                                left-side-width)))
    ;; Early exit if line content unchanged
    (alexandria:when-let ((cached-height (check-line-fingerprint window y fingerprint)))
      (return-from redraw-logical-line-when-horizontal-scroll cached-height))
    (let* ((objects (create-drawing-objects logical-line))
           (height
             (max (max-height-of-objects left-side-objects)
                  (max-height-of-objects objects))))
      (multiple-value-bind (cursor-object cursor-x)
          (find-cursor-object objects)
        (when cursor-object
          (let ((width (- (window-view-width window) left-side-width)))
            (cond ((< cursor-x (horizontal-scroll-start window))
                   (setf (horizontal-scroll-start window) cursor-x))
                  ((< (+ (horizontal-scroll-start window)
                         width)
                      (+ cursor-x (object-width cursor-object)))
                   (setf (horizontal-scroll-start window)
                         (+ (- cursor-x width)
                            (object-width cursor-object)))))))
        (setf objects
              (reduce-objects
               (clip-objects-to-display-range
                objects
                (horizontal-scroll-start window)
                (+ (horizontal-scroll-start window)
                   (window-view-width window)))))
        (render-line-with-caching window 0 y (append left-side-objects objects) height))
      ;; Reuse fingerprint if scroll position didn't change; avoids redundant sxhash
      (update-line-fingerprint
       window y
       (if (eql scroll-before (horizontal-scroll-start window))
           fingerprint
           (compute-line-fingerprint logical-line
                                     (horizontal-scroll-start window)
                                     left-side-width))
       height)
      height)))

(defun redraw-lines (window)
  (let* ((*line-wrap* (variable-value 'line-wrap
                                      :default (window-buffer window)))
         (redraw-fn (if *line-wrap*
                        #'redraw-logical-line-when-line-wrapping
                        #'redraw-logical-line-when-horizontal-scroll)))
    (let ((y 0)
          (height (window-view-height window))
          left-side-width)
      (block outer
        (do-logical-line (logical-line window)
          (let* ((left-side-objects
                   (alexandria:when-let (content (logical-line-left-content logical-line))
                     (mapcan #'create-drawing-object
                             (compute-items-from-string-and-attributes
                              (lem/buffer/line:content-string content)
                              (lem/buffer/line:content-attributes content))))))
            (setf left-side-width
                  (loop :for object :in left-side-objects
                        :sum (object-width object)))
            (incf y (funcall redraw-fn window y logical-line left-side-objects left-side-width))
            (unless (< y height)
              (return-from outer)))))
      (when (< y height)
        (clear-line-fingerprint-cache-from window y)
        (invalidate-drawing-cache-from window y)
        (lem-if:clear-to-end-of-window (implementation) (window-view window) y))
      (setf (window-left-width window)
            (floor left-side-width (lem-if:get-char-width (implementation)))))))

(defun call-with-display-error (function)
  (handler-bind ((error (lambda (e)
                          (log:error "~A"
                                     (with-output-to-string (out)
                                       (format out "~A~%" e)
                                       (uiop:print-backtrace :stream out :condition e)))
                          (message "~A" e)
                          (return-from call-with-display-error))))
    (funcall function)))

(defmacro with-display-error (() &body body)
  `(call-with-display-error (lambda () ,@body)))

(defun make-modeline-objects (window default-attribute)
  (let ((left-objects '())
        (right-objects '()))
    (modeline-apply window
                    (lambda (string attribute alignment)
                      (case alignment
                        ((:right)
                         (alexandria:nconcf
                          right-objects
                          (create-drawing-object
                           (make-string-with-attribute-item :string string
                                                            :attribute attribute))))
                        (otherwise
                         (alexandria:nconcf left-objects
                                            (create-drawing-object
                                             (make-string-with-attribute-item :string string
                                                                              :attribute attribute))))))
                    default-attribute)
    (values left-objects
            right-objects)))

(defun redraw-modeline (window force)
  (declare (ignore force))
  ;; TODO: cache
  (when (window-use-modeline-p window)
    (let* ((view (window-view window))
           (default-attribute (ensure-attribute
                               (if (eq window (current-window))
                                   'modeline
                                   'modeline-inactive))))
      (multiple-value-bind (left-objects right-objects)
          (make-modeline-objects window default-attribute)
        (lem-if:render-line-on-modeline (implementation)
                                        view
                                        left-objects
                                        right-objects
                                        default-attribute
                                        (max (max-height-of-objects left-objects)
                                             (max-height-of-objects right-objects)))))))

(defun get-background-color-of-window (window)
  (cond ((typep window 'floating-window)
         (floating-window-background-color window))
        ((eq window (current-window))
         nil)
        ((eq window (window-parent (current-window)))
         nil)
        ((and (inactive-window-background-color)
              (eq 'window (type-of window)))
         (inactive-window-background-color))
        (t nil)))

(defmethod redraw-buffer :around (implementation buffer window force)
  (with-display-error ()
    (lem-if:redraw-view-before (implementation)
                               (window-view window))
    (let ((lem-if:*background-color-of-drawing-window*
            (get-background-color-of-window window)))
      (call-next-method))
    (when (window-use-modeline-p window)
      (redraw-modeline window
                       (or (window-need-to-redraw-p window)
                           force)))
    (lem-if:redraw-view-after (implementation)
                              (window-view window))))

(defun clear-cache-if-screen-modified (window force)
  (when (or force (window-need-to-redraw-p window))
    (setf (drawing-cache window) '())
    (clear-line-fingerprint-cache window)))

(defmethod redraw-buffer (implementation (buffer text-buffer) window force)
  (assert (eq buffer (window-buffer window)))
  (clear-cache-if-screen-modified window force)
  (redraw-lines window)
  (finish-redraw window))
