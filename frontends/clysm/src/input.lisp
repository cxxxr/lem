;;;; input.lisp — the keyboard and the mouse
;;;;
;;;; The keys are typed into a text field that stays at the cursor (so that
;;;; an input method shows what is being composed there): a key with a
;;;; modifier or without a character (Return, an arrow...) is taken from
;;;; its keydown event, characters from the input events of the field, and
;;;; a composed text when the composition ends.  Each becomes a key of Lem
;;;; sent to the editor.

(in-package :lem-clysm)

(defparameter *key-names*
  '(("Enter" . "Return") ("ArrowUp" . "Up") ("ArrowDown" . "Down")
    ("ArrowLeft" . "Left") ("ArrowRight" . "Right") (" " . "Space")))

(defparameter *modifier-keys* '("Shift" "Control" "Alt" "Meta" "CapsLock" "Hyper" "Super"
                                "OS" "AltGraph" "Process" "Dead" "Unidentified"))

(defun make-lem-key (name &key ctrl meta super shift)
  (let* ((sym (or (cdr (assoc name *key-names* :test #'string=)) name))
         (insertion (lem:insertion-key-sym-p sym)))
    (lem:make-key :ctrl ctrl :meta meta :super super
                  ;; Shift is in the character itself, except with Meta
                  :shift (if insertion nil shift)
                  :sym (if (and insertion shift meta) (string-upcase sym) sym))))

(defun send-key (key)
  (lem:send-event key))

(defun send-string (string)
  (loop for char across string
        do (send-key (if (char= char #\Newline)
                         (make-lem-key "Return")
                         (make-lem-key (string char))))))

;;; the text field

(ui:defstate *composing* nil "True while an input method composes a text.")

(defun event-modifiers (event)
  (values (eq (js:get event "ctrlKey") t)
          (eq (js:get event "altKey") t)
          (eq (js:get event "metaKey") t)
          (eq (js:get event "shiftKey") t)))

(defun on-keydown (event)
  (let ((key (js:get event "key")))
    (unless (or *composing*
                (eq (js:get event "isComposing") t)
                (not (stringp key))
                (member key *modifier-keys* :test #'string=))
      (multiple-value-bind (ctrl alt meta shift) (event-modifiers event)
        ;; a character typed without a modifier comes as an input event, and
        ;; the keys that paste (Command-V, Control-Shift-V, Shift-Insert) are
        ;; left to the browser, which sends a paste event
        (unless (or (and (not ctrl) (not alt) (not meta) (= (length key) 1))
                    (and (string-equal key "v") (not alt) (or (and meta (not ctrl)) (and ctrl shift)))
                    (and (string= key "Insert") shift (not ctrl) (not alt) (not meta)))
          (js:call event "preventDefault")
          (send-key (make-lem-key key :ctrl ctrl :meta alt :super meta :shift shift)))))))

(defun clear-field (event)
  (js:set (js:get event "target") "value" ""))

(defun on-input (event)
  (unless (or *composing* (eq (js:get event "isComposing") t))
    (let ((text (js:get (js:get event "target") "value")))
      (clear-field event)
      (when (and (stringp text) (plusp (length text)))
        (send-string text)))))

(defun on-composition-start (event)
  (declare (ignore event))
  (setf *composing* t))

(defun on-composition-end (event)
  (setf *composing* nil)
  (let ((text (js:get event "data")))
    (clear-field event)
    (when (and (stringp text) (plusp (length text)))
      (send-string text))))

(defun on-paste (event)
  (js:call event "preventDefault")
  (let* ((data (js:get event "clipboardData"))
         (text (and data (js:call data "getData" "text/plain"))))
    (when (and (stringp text) (plusp (length text)))
      (lem:send-event (lambda () (paste-text text))))))

(defun paste-text (text)
  (lem:paste-using-mode (lem:ensure-mode-object
                         (lem:current-major-mode-at-point (lem:current-point)))
                        text)
  (lem:redraw-display))

(defvar *input-field* nil "The DOM element of the text field.")

(defun focus-input ()
  (when *input-field* (js:call *input-field* "focus")))

(ui:defcomponent input-field ()
  (destructuring-bind (column line) *cursor*
    (:textarea :class (if *composing* "lem-input composing" "lem-input") :autocomplete "off" :spellcheck "false"
               :autocapitalize "off" :rows 1
               :style (format nil "left:~A;top:~A;height:~A;font-size:~Apx;"
                              (px (* column *char-width*)) (px (* line *char-height*))
                              (px *char-height*) *font-size*)
               :on-mount (lambda (node) (setf *input-field* node) (js:call node "focus"))
               :on-keydown #'on-keydown
               :on-input #'on-input
               :on-compositionstart #'on-composition-start
               :on-compositionend #'on-composition-end
               :on-paste #'on-paste
               :on-blur (lambda (event)
                          (declare (ignore event))
                          ;; the editor keeps the keyboard
                          (js:funcall *set-timeout* (lambda () (focus-input) nil) 0)))))

;;; the mouse: positions on the screen are cells

(defvar *screen-element* nil "The DOM element of the screen.")

(sys::%defvar-host *element-position*
  (js:eval "(element, event) => { const r = element.getBoundingClientRect();
                                 return [event.clientX - r.left, event.clientY - r.top]; }"))

(defun event-cell (event)
  "The column, line and pixel position of the mouse event EVENT on the screen."
  (let* ((position (js:funcall *element-position* *screen-element* event))
         (px (js:get position 0))
         (py (js:get position 1)))
    (values (max 0 (floor px *char-width*)) (max 0 (floor py *char-height*))
            (round px) (round py))))

(defun mouse-button (event)
  (case (js:get event "button")
    (0 :button-1) (1 :button-2) (2 :button-3) (t nil)))

(defun on-mouse-down (event)
  (let ((button (mouse-button event)))
    (when button
      (js:call event "preventDefault")
      (multiple-value-bind (x y px py) (event-cell event)
        (lem:receive-mouse-button-down x y px py button (max 1 (js:get event "detail")))))
    (focus-input)))

(defun on-mouse-up (event)
  (let ((button (mouse-button event)))
    (when button
      (multiple-value-bind (x y px py) (event-cell event)
        (lem:receive-mouse-button-up x y px py button)))))

(defvar *mouse-position* '(0 0) "The cell of the mouse, for LEM-IF:GET-MOUSE-POSITION.")

(defun on-mouse-move (event)
  (multiple-value-bind (x y px py) (event-cell event)
    (unless (equal *mouse-position* (list x y))
      (setf *mouse-position* (list x y))
      (let ((button (and (plusp (js:get event "buttons")) :button-1)))
        (lem:receive-mouse-motion x y px py button)))))

(defun on-wheel (event)
  (js:call event "preventDefault")
  (multiple-value-bind (x y px py) (event-cell event)
    (let ((dy (js:get event "deltaY")) (dx (js:get event "deltaX")))
      (lem:receive-mouse-wheel x y px py
                               (cond ((plusp dx) 1) ((minusp dx) -1) (t 0))
                               (cond ((plusp dy) -1) ((minusp dy) 1) (t 0)))
      (lem:send-event (lambda ()
                        (when (= 0 (lem:event-queue-length))
                          (lem:redraw-display)))))))
