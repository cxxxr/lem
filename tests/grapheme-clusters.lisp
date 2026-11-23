(defpackage :lem-tests/grapheme-clusters
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/grapheme-clusters)

(defparameter *family* (map 'string #'code-char (list #x1F468 #x200D #x1F469 #x200D #x1F467)))
(defparameter *thumbs-up-medium* (map 'string #'code-char (list #x1F44D #x1F3FD)))
(defparameter *two-flags* (map 'string #'code-char (list #x1F1EF #x1F1F5 #x1F1FA #x1F1F8)))
(defparameter *red-heart* (map 'string #'code-char (list #x2764 #xFE0F)))
(defparameter *keycap-one* (map 'string #'code-char (list #x31 #xFE0F #x20E3)))

(deftest widths-by-grapheme-cluster
  (let ((lem:*grapheme-clusters* t))
    (ok (= 2 (lem:string-width *family*)) "a joined family is one emoji")
    (ok (= 2 (lem:string-width *thumbs-up-medium*)) "so is an emoji with a skin tone")
    (ok (= 4 (lem:string-width *two-flags*)) "two flags are two emoji")
    (ok (= 2 (lem:string-width *red-heart*)) "a text symbol in its emoji form")
    (ok (= 2 (lem:string-width *keycap-one*)) "a keycap")
    (ok (= 4 (lem:string-width (concatenate 'string "a" *thumbs-up-medium* "b"))))))

(deftest widths-per-character-when-off
  (let ((lem:*grapheme-clusters* nil))
    (ok (= 8 (lem:string-width *family*)) "unchanged: each character counts")))

(deftest flags-pair-their-letters
  (ng (lem:grapheme-continuation-p *two-flags* 0))
  (ok (lem:grapheme-continuation-p *two-flags* 1) "the second letter of the first flag")
  (ng (lem:grapheme-continuation-p *two-flags* 2) "the first letter of the second flag")
  (ok (lem:grapheme-continuation-p *two-flags* 3)))

(defun call-with-text (text function)
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (lem:make-buffer "graphemes" :temporary t))
            (lem:*grapheme-clusters* t))
        (lem:switch-to-buffer buffer)
        (lem:insert-string (lem:buffer-point buffer) text)
        (lem:buffer-start (lem:buffer-point buffer))
        (funcall function)))))

(deftest the-cursor-steps-over-a-whole-sequence
  (call-with-text
   (concatenate 'string "a" *family* "b")
   (lambda ()
     (lem:forward-char 1)
     (ok (= 1 (lem:point-charpos (lem:current-point))) "onto the family")
     (lem:forward-char 1)
     (ok (= 6 (lem:point-charpos (lem:current-point))) "past all five characters at once")
     (lem:backward-char 1)
     (ok (= 1 (lem:point-charpos (lem:current-point))) "and back over it at once"))))

(deftest deleting-takes-the-whole-sequence
  (call-with-text
   (concatenate 'string "a" *family* "b")
   (lambda ()
     (lem:character-offset (lem:current-point) 6)
     (lem:delete-previous-char)
     (ok (equal "ab" (lem:line-string (lem:current-point))) "backspace removes the family")
     (lem:insert-string (lem:current-point) *thumbs-up-medium*)
     (lem:character-offset (lem:current-point) -2)
     (lem:delete-next-char)
     (ok (equal "ab" (lem:line-string (lem:current-point)))
         "delete removes the emoji with its skin tone"))))
