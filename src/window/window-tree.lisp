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

(defun balanced-window-layout (&optional (window-tree (window-tree)))
  "Return where balance-windows would put each window of WINDOW-TREE, as a
list of (WINDOW X Y WIDTH HEIGHT), without changing any.  Each split shares
its space evenly between its children."
  (let ((layout '()))
    (labels ((rec (node x y width height)
               (cond ((windowp node)
                      (push (list node x y width height) layout))
                     (t
                      (destructuring-bind (split-type &rest children) node
                        (let ((n (length children)))
                          (ecase split-type
                            (:hsplit
                             (multiple-value-bind (child-width remaining) (floor width n)
                               (loop :with rest-width := width
                                     :for (child . rest-children) :on children
                                     :do (let ((child-width child-width))
                                           (cond ((null rest-children)
                                                  (setf child-width rest-width))
                                                 ((plusp remaining)
                                                  (decf remaining)
                                                  (incf child-width)))
                                           (rec child
                                                x
                                                y
                                                (if (null rest-children)
                                                    child-width
                                                    (- child-width
                                                       (frame-window-left-margin (current-frame))))
                                                height)
                                           (incf x child-width)
                                           (decf rest-width child-width)))))
                            (:vsplit
                             (multiple-value-bind (child-height rem) (floor height n)
                               (loop :with rest-height := height
                                     :for (child . rest-children) :on children
                                     :do (let ((child-height child-height))
                                           (cond ((null rest-children)
                                                  (setf child-height rest-height))
                                                 ((plusp rem)
                                                  (decf rem)
                                                  (incf child-height)))
                                           (rec child
                                                x
                                                y
                                                width
                                                (if (null rest-children)
                                                    child-height
                                                    (- child-height
                                                       (if (frame-enable-window-modeline-per-window (current-frame))
                                                           0
                                                           (frame-window-bottom-margin (current-frame))))))
                                           (incf y child-height)
                                           (decf rest-height child-height))))))))))))
      (rec (convert-window-n-tree window-tree)
           (topleft-window-x (current-frame))
           (topleft-window-y (current-frame))
           (max-window-width (current-frame))
           (max-window-height (current-frame))))
    (nreverse layout)))

(defun window-layout-fits-p (layout)
  "True if every window in LAYOUT, a list of (WINDOW X Y WIDTH HEIGHT), is
wide enough and shows at least one line besides its modeline.  It does not
when the display is smaller than the windows need, e.g. while the user
shrinks the editor's window."
  (loop :for (window nil nil width height) :in layout
        :always (and (valid-window-width-p width)
                     (valid-window-height-p
                      (if (window-use-modeline-p window) (1- height) height)))))

(defun balance-windows (&optional (window-tree (window-tree)))
  "Share the frame's space evenly between the windows of WINDOW-TREE, and
return true.  If the space is too small for every window to fit (see
window-layout-fits-p), leave them as they are and return NIL: they are drawn
clipped until the display grows."
  (let ((layout (balanced-window-layout window-tree)))
    (when (window-layout-fits-p layout)
      (loop :for (window x y width height) :in layout
            :do (window-set-size window width height)
                (window-set-pos window x y))
      t)))
