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

(defun distribute-size (total current fixed proportional)
  "Share TOTAL among children whose sizes are CURRENT: those that are FIXED
(a list of booleans) keep their size, the others share the rest, equally or
(PROPORTIONAL) as they shared it.  If the fixed ones leave the others less
than two lines (or columns) each, none is fixed."
  (let* ((flexible (count nil fixed))
         (rest (- total (loop :for size :in current :for f :in fixed :when f :sum size))))
    (when (or (zerop flexible) (< rest (* 2 flexible)))
      (setf fixed (make-list (length current) :initial-element nil)
            flexible (length current)
            rest total))
    (let* ((weights (loop :for size :in current :for f :in fixed
                          :unless f :collect (if proportional (max 1 size) 1)))
           (sum (reduce #'+ weights))
           (shares (mapcar (lambda (weight) (floor (* rest weight) sum)) weights))
           (left (- rest (reduce #'+ shares))))
      ;; what the rounding left, one by one from the first
      (loop :for cell :on shares
            :while (plusp left)
            :do (incf (car cell))
                (decf left))
      (loop :for size :in current :for f :in fixed
            :collect (if f size (pop shares))))))

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
                          (sizes (distribute-size total
                                                  (current-slot-sizes children split-type)
                                                  (mapcar (lambda (child)
                                                            (window-size-fixed-in-p child split-type))
                                                          children)
                                                  proportional)))
                     (loop :for (child . rest-children) :on children
                           :for size :in sizes
                           :for inner := (if rest-children (- size margin) size)
                           :do (ecase split-type
                                 (:hsplit (rec child x y inner height)
                                          (incf x size))
                                 (:vsplit (rec child x y width inner)
                                          (incf y size)))))))))
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
