(defpackage :lem-ncurses/input
  (:use :cl
        :lem
        :lem-ncurses/key)
  (:export :get-event))
(in-package :lem-ncurses/input)

;; for input
;;  (we don't use stdscr for input because it calls wrefresh implicitly
;;   and causes the display confliction by two threads)
(defvar *padwin* nil)

(defparameter *paste-end* #(27 91 50 48 49 126)
  "ESC [ 2 0 1 ~, which ends a bracketed paste.")

(defun getch ()
  (unless *padwin*
    (setf *padwin* (charms/ll:newpad 1 1))
    (charms/ll:keypad *padwin* 1)
    (charms/ll:wtimeout *padwin* -1))
  (charms/ll:wgetch *padwin*))

(defmacro with-getch-input-timeout ((time) &body body)
  `(progn
     (charms/ll:wtimeout *padwin* ,time)
     (unwind-protect (progn ,@body)
       (charms/ll:wtimeout *padwin* -1))))

(defun utf8-bytes (c)
  (cond
    ((<= c #x7f) 1)
    ((<= #xc2 c #xdf) 2)
    ((<= #xe0 c #xef) 3)
    ((<= #xf0 c #xf4) 4)
    (t 1)))

(defun read-multibyte-char (code nbytes)
  (let ((vec (make-array nbytes :element-type '(unsigned-byte 8))))
    (setf (aref vec 0) code)
    (with-getch-input-timeout (100)
      (loop :for i :from 1 :below nbytes
            :do (setf (aref vec i) (getch))))
    (handler-case (schar (babel:octets-to-string vec :encoding :utf-8) 0)
      (babel-encodings:character-decoding-error ()
        (code-char code)))))

(defun read-pasted-text ()
  "Read a bracketed paste after its ESC[200~ up to ESC[201~ and return it as
a string.  Line breaks arrive as CR and are returned as newlines.  Stops at
a second of silence, in case the end marker is lost."
  (let ((octets (make-array 256 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0))
        (end-length (length *paste-end*)))
    (with-getch-input-timeout (1000)
      (loop :for code := (getch)
            :until (= code -1)
            :when (<= 0 code 255)
              :do (vector-push-extend (if (= code 13) 10 code) octets)
                  (when (and (>= (fill-pointer octets) end-length)
                             (equalp *paste-end*
                                     (subseq octets (- (fill-pointer octets) end-length))))
                    (decf (fill-pointer octets) end-length)
                    (return))))
    (babel:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                            :encoding :utf-8 :errorp nil)))

(defun paste-event (text)
  "An event that pastes TEXT with the mode at point, as the server frontend
does for clipboard pastes: a terminal buffer sends it to its program as a
bracketed paste, other buffers insert it as one yank."
  (lambda ()
    ;; Function events run outside the command loop, so add the undo
    ;; boundaries and the redisplay a command would get.
    (let ((buffer (current-buffer)))
      (buffer-undo-boundary buffer)
      (paste-using-mode (ensure-mode-object
                         (current-major-mode-at-point (current-point)))
                        text)
      (buffer-undo-boundary buffer))
    (redraw-display)))

(defun csi\[2 ()
  "After ESC [ 2: a bracketed paste (ESC[200~) or something we don't handle."
  (if (and (= (getch) #.(char-code #\0))
           (= (getch) #.(char-code #\0))
           (= (getch) #.(char-code #\~)))
      (paste-event (read-pasted-text))
      (get-key-from-name "escape")))

(defun get-key (code)
  (let ((nbytes (utf8-bytes code)))
    (if (= nbytes 1)
        (char-to-key (code-char code))
        ;; A multibyte sequence is always a character, never an ncurses
        ;; keycode, so skip *keycode-table*: its KEY_ values (258-567)
        ;; collide with Latin Extended code points such as č (269 = [f5])
        ;; and š (353 = [btab]). See #656.
        (make-key :sym (string (read-multibyte-char code nbytes))))))

(defun csi\[1 ()
  (or (case (getch)
        (#.(char-code #\;)
           (case (getch)
             (#.(char-code #\2)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :sym "Home"))))
             (#.(char-code #\3)
                (case (getch)
                  (#.(char-code #\A) (make-key :meta t :sym "Up"))
                  (#.(char-code #\B) (make-key :meta t :sym "Down"))
                  (#.(char-code #\C) (make-key :meta t :sym "Right"))
                  (#.(char-code #\D) (make-key :meta t :sym "Left"))
                  (#.(char-code #\F) (make-key :meta t :sym "End"))
                  (#.(char-code #\H) (make-key :meta t :sym "Home"))))
             (#.(char-code #\4)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :meta t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :meta t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :meta t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :meta t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :meta t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :meta t :sym "Home"))))
             (#.(char-code #\5)
                (case (getch)
                  (#.(char-code #\A) (make-key :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :ctrl t :sym "Home"))))
             (#.(char-code #\6)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :ctrl t :sym "Home"))))
             (#.(char-code #\7)
                (case (getch)
                  (#.(char-code #\A) (make-key :meta t :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :meta t :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :meta t :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :meta t :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :meta t :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :meta t :ctrl t :sym "Home"))))
             (#.(char-code #\8)
                (case (getch)
                  (#.(char-code #\A) (make-key :shift t :meta t :ctrl t :sym "Up"))
                  (#.(char-code #\B) (make-key :shift t :meta t :ctrl t :sym "Down"))
                  (#.(char-code #\C) (make-key :shift t :meta t :ctrl t :sym "Right"))
                  (#.(char-code #\D) (make-key :shift t :meta t :ctrl t :sym "Left"))
                  (#.(char-code #\F) (make-key :shift t :meta t :ctrl t :sym "End"))
                  (#.(char-code #\H) (make-key :shift t :meta t :ctrl t :sym "Home")))))))
      (get-key-from-name "escape")))

(let ((resize-code (get-code "[resize]"))
      (abort-code (get-code "C-]"))
      (escape-code (get-code "escape")))
  (defun get-event ()
    (tagbody :start
      (return-from get-event
        (let ((code (getch)))
          (cond ((= code -1) (go :start))
                ((= code resize-code) :resize)
                ((= code abort-code) :abort)
                ((= code escape-code)
                 (let ((code (with-getch-input-timeout
                                 ((variable-value 'lem-ncurses/config:escape-delay))
                               (getch))))
                   (cond ((= code -1)
                          (get-key-from-name "escape"))
                         ((= code #.(char-code #\[))
                          (with-getch-input-timeout (100)
                            (case (getch)
                              (#.(char-code #\<)
                                 ;;sgr(1006)
                                 (uiop:symbol-call :lem-mouse-sgr1006
                                                   :parse-mouse-event
                                                   #'getch))
                              (#.(char-code #\1)
                                 (csi\[1))
                              (#.(char-code #\2)
                                 (csi\[2))
                              (t (get-key-from-name "escape")))))
                         (t
                          (let ((key (get-key code)))
                            (make-key :meta t
                                      :sym (key-sym key)
                                      :ctrl (key-ctrl key)))))))
                (t
                 (get-key code))))))))
