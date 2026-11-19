(in-package :lem-core)

(defun window-tree ()
  (frame-window-tree (current-frame)))

(defun (setf window-tree) (new-window-tree)
  (let ((frame (current-frame)))
    (setf (frame-window-tree frame) new-window-tree)))

(defstruct (window-node (:constructor %make-window-node))
  split-type
  children)

(defun window-node-left (window-node)
  (elt (window-node-children window-node) 0))

(defun (setf window-node-left) (value window-node)
  (setf (elt (window-node-children window-node) 0) value))

(defun window-node-right (window-node)
  (elt (window-node-children window-node) 1))

(defun (setf window-node-right) (value window-node)
  (setf (elt (window-node-children window-node) 1) value))

(defun make-window-node (split-type left right)
  (assert (member split-type '(:hsplit :vsplit)))
  (%make-window-node :split-type split-type
                     :children (make-array 2 :initial-contents (list left right))))

(defun window-tree-leaf-p (window)
  (windowp window))

(defun window-tree-map (tree fn)
  (labels ((f (tree)
             (if (window-tree-leaf-p tree)
                 (funcall fn tree)
                 (map nil #'f (window-node-children tree)))))
    (f tree)
    nil))

(defun window-tree-flatten (tree)
  (let ((windows))
    (window-tree-map tree
                     (lambda (win)
                       (push win windows)))
    (nreverse windows)))

(defun window-tree-find (tree window)
  (window-tree-map tree
                   (lambda (win)
                     (when (eq win window)
                       (return-from window-tree-find win)))))

(defun window-tree-find-if (tree test)
  (window-tree-map tree
                   (lambda (win)
                     (when (funcall test win)
                       (return-from window-tree-find-if win)))))

(defun find-node-with-window (tree window)
  (labels ((recursive (node)
             (cond ((typep node 'window) nil)
                   ((find window (window-node-children node))
                    node)
                   (t
                    (some #'recursive (window-node-children node))))))
    (recursive tree)))

(defun get-parent-window-node-accessors (tree node)
  (cond ((window-tree-leaf-p tree) nil)
        ((eq node (window-node-left tree))
         (values tree
                 (lambda ()
                   (window-node-left tree))
                 (lambda (new-value)
                   (setf (window-node-left tree) new-value))
                 (lambda ()
                   (window-node-right tree))
                 (lambda (new-value)
                   (setf (window-node-right tree) new-value))))
        ((eq node (window-node-right tree))
         (values tree
                 (lambda ()
                   (window-node-right tree))
                 (lambda (new-value)
                   (setf (window-node-right tree) new-value))
                 (lambda ()
                   (window-node-left tree))
                 (lambda (new-value)
                   (setf (window-node-left tree) new-value))))
        (t
         (multiple-value-bind (parent
                               getter
                               setter
                               another-getter
                               another-setter)
             (get-parent-window-node-accessors (window-node-left tree) node)
           (if parent
               (values parent getter setter another-getter another-setter)
               (get-parent-window-node-accessors (window-node-right tree) node))))))

(defun convert-window-n-tree (node)
  (labels ((join-children (node child)
             (cond ((windowp child)
                    (list child))
                   ((and (window-node-p child)
                         (eq (window-node-split-type node)
                             (window-node-split-type child)))
                    (rec child))
                   (t
                    (list (convert-window-n-tree child)))))
           (rec (node)
             (cond ((windowp node)
                    (list node))
                   (t
                    (append (join-children node (window-node-left node))
                            (join-children node (window-node-right node)))))))
    (if (windowp node)
        node
        (cons (window-node-split-type node)
              (rec node)))))

(defun window-size-fixed-in-p (node split-type)
  "Whether NODE (a window or a subtree) keeps its size along SPLIT-TYPE: a
window whose height (:vsplit) or width (:hsplit) is fixed."
  (and (windowp node)
       (ecase split-type
         (:vsplit (window-height-fixed-p node))
         (:hsplit (window-width-fixed-p node)))))

(defun window-node-extent (node split-type)
  "Where NODE (a window, or an n-tree of windows) begins and ends along
SPLIT-TYPE: (values start end)."
  (let ((windows (if (windowp node) (list node) (window-tree-flatten-n-tree node))))
    (ecase split-type
      (:hsplit (values (reduce #'min windows :key #'window-x)
                       (reduce #'max windows :key (lambda (w) (+ (window-x w) (window-width w))))))
      (:vsplit (values (reduce #'min windows :key #'window-y)
                       (reduce #'max windows :key (lambda (w) (+ (window-y w) (window-height w)))))))))

(defun window-tree-flatten-n-tree (node)
  (if (windowp node)
      (list node)
      (mapcan #'window-tree-flatten-n-tree (rest node))))

(defun current-slot-sizes (children split-type)
  "The room that each of CHILDREN takes now along SPLIT-TYPE: from where it
begins to where the next one begins (its margin with it), the last to its end."
  (loop :for (child next) :on children
        :collect (if next
                     (- (window-node-extent next split-type)
                        (window-node-extent child split-type))
                     (multiple-value-bind (start end) (window-node-extent child split-type)
                       (- end start)))))

(defun distribute-size (total current fixed weights)
  "Share TOTAL among children whose sizes are CURRENT: those that are FIXED
(a list of booleans) keep their size, the others share the rest as their
WEIGHTS.  If the fixed ones leave the others less than two lines (or columns)
each, none is fixed.  The sizes, and as second value what each would have
had without rounding."
  (let* ((flexible (count nil fixed))
         (rest (- total (loop :for size :in current :for f :in fixed :when f :sum size))))
    (when (or (zerop flexible) (< rest (* 2 flexible)))
      (setf fixed (make-list (length current) :initial-element nil)
            flexible (length current)
            rest total))
    (let* ((weights (loop :for weight :in weights :for f :in fixed
                          :unless f :collect (max 1 weight)))
           (sum (reduce #'+ weights))
           (exact (mapcar (lambda (weight) (/ (* rest weight) sum)) weights))
           (shares (mapcar #'floor exact))
           (left (- rest (reduce #'+ shares))))
      ;; what the rounding left, one by one to those it took the most from
      ;; (not always the first: resizing back and forth would make it grow)
      (dolist (index (stable-sort (loop :for i :from 0 :below (length exact) :collect i)
                                  #'>
                                  :key (lambda (i) (- (nth i exact) (nth i shares)))))
        (when (<= left 0) (return))
        (incf (nth index shares))
        (decf left))
      (values (loop :for size :in current :for f :in fixed
                    :collect (if f size (pop shares)))
              (loop :for size :in current :for f :in fixed
                    :collect (if f size (pop exact)))))))

;;; Fitted to the frame again and again (a window dragged larger), the
;;; windows would drift from their shares as their sizes are rounded: what
;;; each would have had is kept (on its first window, for the split it is in)
;;; and used again as long as its size is still that one rounded.

(defun child-first-window (child)
  (first (window-tree-flatten-n-tree child)))

(defun remembered-share (child key size)
  "The size CHILD had without rounding in the split KEY, if SIZE is still
that one rounded; otherwise SIZE."
  (let ((entry (assoc key (window-parameter (child-first-window child) 'layout-shares)
                      :test #'equal)))
    (if (and entry (< (abs (- (cdr entry) size)) 1))
        (cdr entry)
        size)))

(defun remember-share (child key exact)
  (let* ((window (child-first-window child))
         (shares (remove key (window-parameter window 'layout-shares) :key #'car :test #'equal)))
    (setf (window-parameter window 'layout-shares) (acons key exact shares))))

(defun lay-out-windows (window-tree proportional)
  "Give the windows of WINDOW-TREE the room of the frame: along each split,
the windows whose size is fixed keep it, the others share the rest, equally
or (PROPORTIONAL) as they shared it."
  (labels ((rec (node x y width height)
             (if (windowp node)
                 (progn
                   (window-set-size node width height)
                   (window-set-pos node x y))
                 (destructuring-bind (split-type &rest children) node
                   (let* ((total (ecase split-type (:hsplit width) (:vsplit height)))
                          (margin (ecase split-type
                                    (:hsplit (frame-window-left-margin (current-frame)))
                                    (:vsplit (if (frame-enable-window-modeline-per-window (current-frame))
                                                 0
                                                 (frame-window-bottom-margin (current-frame))))))
                          (key (cons split-type (child-first-window node)))
                          (current (current-slot-sizes children split-type)))
                     (multiple-value-bind (sizes exact)
                         (distribute-size total
                                          current
                                          (mapcar (lambda (child)
                                                    (window-size-fixed-in-p child split-type))
                                                  children)
                                          (if proportional
                                              (mapcar (lambda (child size) (remembered-share child key size))
                                                      children current)
                                              (make-list (length children) :initial-element 1)))
                       (loop :for (child . rest-children) :on children
                             :for size :in sizes
                             :for share :in exact
                             :for inner := (if rest-children (- size margin) size)
                             :do (remember-share child key share)
                                 (ecase split-type
                                   (:hsplit (rec child x y inner height)
                                            (incf x size))
                                   (:vsplit (rec child x y width inner)
                                            (incf y size))))))))))
    (rec (convert-window-n-tree window-tree)
         (topleft-window-x (current-frame))
         (topleft-window-y (current-frame))
         (max-window-width (current-frame))
         (max-window-height (current-frame)))))

(defun balance-windows (&optional (window-tree (window-tree)))
  "Share the room of the frame equally among the windows of WINDOW-TREE,
along each split; a window whose height or width is fixed keeps it."
  (lay-out-windows window-tree nil))

(defun resize-windows-proportionally (&optional (window-tree (window-tree)))
  "Fit the windows of WINDOW-TREE to the room of the frame (after it changed),
each split shared as it was; a window whose height or width is fixed keeps it."
  (lay-out-windows window-tree t))
