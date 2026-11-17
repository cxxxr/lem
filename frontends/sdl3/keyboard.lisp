(defpackage :lem-sdl3/keyboard
  (:use :cl)
  (:local-nicknames (:event :lem-sdl3/event))
  (:export :set-keyboard-layout
           :make-key-event-from-sdl
           :handle-textediting
           :handle-text-input
           :handle-key-down
           :handle-key-up
           :right-alt-is-meta))
(in-package :lem-sdl3/keyboard)

(defstruct (keyinfo (:type list))
  keycode
  sym
  text-input-p)

(defparameter *code-name-table*
  `((,event:+sdlk-backspace+ "Backspace" nil)
    (,event:+sdlk-tab+ "Tab" nil)
    (,event:+sdlk-return+ "Return" nil)
    (,event:+sdlk-insert+ "Insert" nil)
    (,event:+sdlk-delete+ "Delete" nil)
    (,event:+sdlk-space+ "Space" t)
    (,event:+sdlk-home+ "Home" nil)
    (,event:+sdlk-end+ "End" nil)
    (,event:+sdlk-pageup+ "PageUp" nil)
    (,event:+sdlk-pagedown+ "PageDown" nil)
    (,event:+sdlk-escape+ "Escape" nil)
    (,event:+sdlk-application+ "ContextMenu" nil)
    (,event:+sdlk-menu+ "ContextMenu" nil)
    (,event:+sdlk-left+ "Left" nil)
    (,event:+sdlk-right+ "Right" nil)
    (,event:+sdlk-up+ "Up" nil)
    (,event:+sdlk-down+ "Down" nil)
    ,@(loop :for n :from 1 :to 24
            :collect (list (event:sdlk-function-key n) (format nil "F~D" n) nil))))

(defun convert-to-sym (code)
  (let ((keyinfo (assoc code *code-name-table*)))
    (if keyinfo
        (values (keyinfo-sym keyinfo) (keyinfo-text-input-p keyinfo))
        (when (<= code #x110000)
          (values (string (code-char code))
                  t)))))

(defun make-key (&key ctrl meta shift super hyper sym)
  (when (equal sym (string #\yen_sign))
    (setf sym "\\"))
  (lem:make-key :ctrl ctrl
                :meta meta
                :super super
                :hyper hyper
                :shift shift
                :sym sym))

(defstruct modifier
  shift
  ctrl
  meta
  super
  hyper)

(defstruct (key-event (:constructor %make-key-event))
  code
  modifier)

(defun make-key-event (code modifier)
  (%make-key-event :code code :modifier modifier))

(defun make-key-event-from-sdl (keycode keymod)
  "Make a key-event from an SDL_Keycode and SDL_Keymod bitmask."
  (make-key-event keycode (get-modifier keymod)))

(defparameter *modifier-code-table*
  `((:shift ,event:+kmod-lshift+ ,event:+kmod-rshift+)
    (:ctrl ,event:+kmod-lctrl+ ,event:+kmod-rctrl+)
    (:meta ,event:+kmod-lalt+ ,event:+kmod-ralt+)
    (:super ,event:+kmod-lgui+)
    (:hyper ,event:+kmod-rgui+)))

(defun right-alt-is-meta (on)
  (let ((val (if on
                 `(:meta ,event:+kmod-lalt+ ,event:+kmod-ralt+)
                 `(:meta ,event:+kmod-lalt+))))
    (unless (equal (assoc :meta *modifier-code-table*) val)
      (setf *modifier-code-table* (cons val *modifier-code-table*)))))

(defun mod-p (mod mod-type)
  (some (lambda (value)
          (= value (logand value mod)))
        (cdr (assoc mod-type *modifier-code-table*))))

(defun get-modifier (mod)
  (make-modifier :shift (mod-p mod :shift)
                 :ctrl (mod-p mod :ctrl)
                 :meta (mod-p mod :meta)
                 :super (mod-p mod :super)
                 :hyper (mod-p mod :hyper)))

(defun update-modifier (modifier new-modifier)
  (setf (modifier-shift modifier) (modifier-shift new-modifier))
  (setf (modifier-ctrl modifier) (modifier-ctrl new-modifier))
  (setf (modifier-meta modifier) (modifier-meta new-modifier))
  (setf (modifier-super modifier) (modifier-super new-modifier))
  (setf (modifier-hyper modifier) (modifier-hyper new-modifier)))

(defvar *modifier* (make-modifier))
(defvar *textediting-text* "")

(defmethod handle-textediting (platform text)
  (setf *textediting-text* text))

(defun send-key-event (key)
  (if (lem:match-key key :ctrl t :sym "]")
      (lem:send-abort-event (lem:find-editor-thread) nil)
      (lem:send-event key)))

;; linux
(defun modifier-is-accept-text-input-p (modifier)
  (and (not (modifier-ctrl modifier))
       (not (modifier-meta modifier))))

(defmethod handle-text-input ((platform lem-sdl3/platform:linux) text)
  (handle-text-input-unix text))

(defmethod handle-key-down ((platform lem-sdl3/platform:linux) key-event)
  (handle-key-down-unix key-event))

(defmethod handle-key-up ((platform lem-sdl3/platform:linux) key-event)
  (handle-key-up-unix key-event))

(defun handle-text-input-unix (text)
  (when (modifier-is-accept-text-input-p *modifier*)
    (loop :for c :across text
          :do (multiple-value-bind (sym text-input-p) (convert-to-sym (char-code c))
                (let ((key (lem:make-key :ctrl (modifier-ctrl *modifier*)
                                         :meta (modifier-meta *modifier*)
                                         :super (modifier-super *modifier*)
                                         :hyper (modifier-hyper *modifier*)
                                         :shift nil
                                         :sym sym)))
                  (when text-input-p
                    (send-key-event key)))))))

(defun handle-key-down-unix (key-event)
  (let ((modifier (key-event-modifier key-event))
        (code (key-event-code key-event)))
    (update-modifier *modifier* modifier)
    (multiple-value-bind (sym text-input-p) (convert-to-sym code)
      (when (and sym
                 (or (not text-input-p)
                     (not (modifier-is-accept-text-input-p *modifier*))
                     (< 256 code)))
        (let ((key (make-key-with-shift-careful :shift (modifier-shift modifier)
                                                :ctrl (modifier-ctrl modifier)
                                                :meta (modifier-meta modifier)
                                                :super (modifier-super modifier)
                                                :hyper (modifier-hyper modifier)
                                                :sym sym)))
          (send-key-event key))))))

(defun handle-key-up-unix (key-event)
  (update-modifier *modifier* (key-event-modifier key-event)))

;;
(defparameter *us-shift-layout*
  '((#\1 . #\!)
    (#\2 . #\@)
    (#\3 . #\#)
    (#\4 . #\$)
    (#\5 . #\%)
    (#\6 . #\^)
    (#\7 . #\&)
    (#\8 . #\*)
    (#\9 . #\()
    (#\0 . #\))
    (#\- . #\_)
    (#\= . #\+)
    (#\` . #\~)
    (#\[ . #\{)
    (#\] . #\})
    (#\\ . #\|)
    (#\; . #\:)
    (#\' . #\")
    (#\, . #\<)
    (#\. . #\>)
    (#\/ . #\?)))

(defparameter *jis-shift-layout*
  '((#\1 . #\!)
    (#\2 . #\")
    (#\3 . #\#)
    (#\4 . #\$)
    (#\5 . #\%)
    (#\6 . #\&)
    (#\7 . #\')
    (#\8 . #\()
    (#\9 . #\))
    (#\0 . #\0)
    (#\- . #\=)
    (#\^ . #\_)
    (#\\ . #\|)
    (#\@ . #\`)
    (#\[ . #\{)
    (#\] . #\})
    (#\; . #\+)
    (#\: . #\*)
    (#\] . #\})
    (#\, . #\<)
    (#\. . #\>)
    (#\/ . #\?)))

(defparameter *shift-layout* *us-shift-layout*)

(defun set-keyboard-layout (type)
  (check-type type (member :jis :us))
  (ecase type
    (:us
     (setf *shift-layout* *us-shift-layout*))
    (:jis
     (setf *shift-layout* *jis-shift-layout*))))

(defun shift-char (char)
  (if (alpha-char-p char)
      (values (char-upcase char) t)
      (let ((elt (assoc char *shift-layout*)))
        (if elt
            (values (cdr elt) t)
            (values char nil)))))

(defun make-key-with-shift-careful (&key shift ctrl meta super hyper sym)
  (or (and (= 1 (length sym))
           shift
           (multiple-value-bind (char shift-p)
               (shift-char (char sym 0))
             (and shift-p
                  (make-key :ctrl ctrl
                            :meta meta
                            :super super
                            :hyper hyper
                            :shift nil
                            :sym (string char)))))
      (make-key :ctrl ctrl
                :meta meta
                :super super
                :hyper hyper
                :shift shift
                :sym sym)))

(defun handle-text-input-internal (text)
  (unless (or (modifier-hyper *modifier*)
              (modifier-super *modifier*)
              (modifier-meta *modifier*)
              (modifier-ctrl *modifier*))
    (loop :for c :across text
          :do (let ((key (make-key :ctrl (modifier-ctrl *modifier*)
                                   :meta (modifier-meta *modifier*)
                                   :super (modifier-super *modifier*)
                                   :hyper (modifier-hyper *modifier*)
                                   :shift nil
                                   :sym (convert-to-sym (char-code c)))))
                (send-key-event key)))))

(defun handle-key-down-internal (key-event)
  (let ((code (key-event-code key-event))
        (modifier (key-event-modifier key-event)))
    (update-modifier *modifier* modifier)
    (when (equal *textediting-text* "")
      (multiple-value-bind (sym text-input-p) (convert-to-sym code)
        (when (and sym
                   (or (not text-input-p)
                       (modifier-ctrl modifier)
                       (modifier-meta modifier)
                       (modifier-super modifier)
                       (modifier-hyper modifier)
                       (< 256 code)))
          (let ((key (make-key-with-shift-careful :shift (modifier-shift modifier)
                                                  :ctrl (modifier-ctrl modifier)
                                                  :meta (modifier-meta modifier)
                                                  :super (modifier-super modifier)
                                                  :hyper (modifier-hyper modifier)
                                                  :sym sym)))
            (send-key-event key)))))))

(defun handle-key-up-internal (key-event)
  (update-modifier *modifier* (key-event-modifier key-event)))

(defmethod handle-text-input ((platform lem-sdl3/platform:mac) text)
  (handle-text-input-internal text))

(defmethod handle-key-down ((platform lem-sdl3/platform:mac) key-event)
  (handle-key-down-internal key-event))

(defmethod handle-key-up ((platform lem-sdl3/platform:mac) key-event)
  (handle-key-up-internal key-event))

;; windows
(defmethod handle-text-input ((platform lem-sdl3/platform:windows) text)
  (handle-text-input-internal text))

(defmethod handle-key-down ((platform lem-sdl3/platform:windows) key-event)
  (handle-key-down-internal key-event))

(defmethod handle-key-up ((platform lem-sdl3/platform:windows) key-event)
  (handle-key-up-internal key-event))
