(defpackage :lem-tests/visual-line
  (:use :cl :rove :lem)
  (:import-from
   :lem-tests/utilities
   :with-testing-buffer
   :make-text-buffer
   :lines))

(in-package :lem-tests/visual-line)

(defun point-at-line (buffer line-number)
  "Temporary point at the start of LINE-NUMBER (0-based) of BUFFER."
  (let ((point (copy-point (buffer-start-point buffer) :temporary)))
    (line-offset point line-number)
    (line-start point)
    point))

(defun forward-char-command ()
  (lem-core::save-continue-flags
   (lem-core:call-command (make-instance 'lem:forward-char) nil)))

(defun backward-char-command ()
  (lem-core::save-continue-flags
   (lem-core:call-command (make-instance 'lem:backward-char) nil)))

(defun next-line-command ()
  (lem-core::save-continue-flags
   (lem-core:call-command (make-instance 'lem:next-line) nil)))

(deftest visual-line-navigation-across-folds
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer
                                  (lines "AAAA" "BBBB" "CCCC" "DDDD" "EEEE" "FFFF")))
      (labels ((fold-lines (first last)
                 "Fold buffer lines FIRST..LAST (inclusive) into a single visual line."
                 (place-region-placeholder-overlay
                  (point-at-line buffer first)
                  (point-at-line buffer (1+ last))))
               (expect-visual-line (containing-line first-line last-line)
                 (testing (format
                           nil
                           "Buffer line ~D belongs to the visual line spanning lines ~D..~D"
                           containing-line
                           first-line
                           last-line)
                   (with-point ((p1 (point-at-line buffer containing-line))
                                (p2 (point-at-line buffer containing-line)))
                     (line-offset p1 0 2)
                     (line-offset p2 0 2)
                     (ok (= (position-at-point (visual-line-beginning p1))
                            (position-at-point (point-at-line buffer first-line))))
                     (ok (= (position-at-point (visual-line-end p2))
                            (position-at-point (line-end (point-at-line buffer last-line)))))))))
        (fold-lines 0 1)
        (fold-lines 3 4)
        ;; folded lines collapse with others, unfolded lines stand alone.
        (expect-visual-line 0 0 1)
        (expect-visual-line 1 0 1)
        (expect-visual-line 2 2 2)
        (expect-visual-line 3 3 4)
        (expect-visual-line 4 3 4)
        (expect-visual-line 5 5 5)))))

(defun hide-range (buffer start-charpos end-charpos &key (cursor-behavior nil))
  (with-point ((start (buffer-start-point buffer))
               (end (buffer-start-point buffer)))
    (character-offset start start-charpos)
    (character-offset end end-charpos)
    (place-region-placeholder-overlay
     start
     end
     :is-line-fold nil
     :cursor-behavior cursor-behavior)))

;; move-point-out-of-overlay (the :move-out handler) moves the cursor out of an invisible
;; overlay's span
(deftest cursor-moves-out-of-invisible-overlay
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer (lines "ABCDEFGH")))
      (lem-core::set-window-buffer buffer (current-window))
      ;; hides "EFG"
      (let ((overlay (hide-range buffer 4 7 :cursor-behavior :move-out))
            (point (buffer-point buffer)))
        (move-point point (buffer-start-point buffer))
        ;; ABC|DEFGH, just before the range
        (character-offset point 3)
        ;; ABCD|EFGH, enters the range, gets moved out
        (forward-char-command)
        (ok (null (invisible-overlay-covering point)) "not left on hidden text")
        (ok (point= point (overlay-end overlay)) "pushed forward out of the span")
        ;; entering from the far side throws the cursor out the other way
        (backward-char-command)
        (ok (null (invisible-overlay-covering point)) "not left on hidden text")
        (ok (point< point (overlay-start overlay)) "pushed backward out of the span")))))

;; reveal-overlay-on-cursor-enter / hide-overlay-on-cursor-leave toggle :invisible as the cursor
;; enters and leaves.
(deftest cursor-reveals-and-hides-invisible-overlay
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer (lines "ABCDEFGH")))
      (lem-core::set-window-buffer buffer (current-window))
      (let ((overlay (hide-range buffer 4 7 :cursor-behavior :reveal))
            (point (buffer-point buffer)))
        (ok (eq t (overlay-get overlay :invisible)) "starts hidden")
        ;; step from just before the hidden range into it; entering must reveal it.
        (move-point point (buffer-start-point buffer))
        ;; ABC|DEFGH, just before the range
        (character-offset point 3)
        ;; ABCD|EFGH, cursor enters the range, text gets revealed
        (forward-char-command)
        (ok (null (overlay-get overlay :invisible)) "revealed on enter")
        ;; keep walking until the cursor passes the far edge. leaving must hide it again.
        (loop :until (point<= (overlay-end overlay) point)
              :do (forward-char-command))
        (ok (eq t (overlay-get overlay :invisible)) "hidden again on leave")))))

;; move-point-out-of-overlay also moves the cursor out of a :move-out fold whose hidden span
;; crosses a newline (an arbitrary line-collapsing fold), so forward-char/backward-char never get
;; stuck on the hidden text when stepping in from either side.
(deftest cursor-moves-out-of-newline-crossing-fold
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer
                                  (lines "abc DEF ghi" "JKL mno PQR" "stu VWX yz")))
      (lem-core::set-window-buffer buffer (current-window))
      ;; hide "DEF ghi" + newline + "JKL "
      (with-point ((start (point-at-line buffer 0))
                   (end (point-at-line buffer 1)))
        (character-offset start 4)
        (character-offset end 4)
        (let ((overlay (place-region-placeholder-overlay
                        start
                        end
                        :is-line-fold nil
                        :cursor-behavior :move-out))
              (point (buffer-point buffer)))
          ;; "abc |DEF..." just before the span. entering it pushes out the forward edge.
          (move-point point (point-at-line buffer 0))
          (character-offset point 3)
          (forward-char-command)
          (ok (null (invisible-overlay-covering point)) "not left on hidden text")
          (ok (point= point (overlay-end overlay)) "pushed forward out of the span")
          ;; entering from the far side throws the cursor out the other way
          (backward-char-command)
          (ok (null (invisible-overlay-covering point)) "not left on hidden text")
          (ok (point< point (overlay-start overlay)) "pushed backward out of the span"))))))

;; vertical motion steps clear of a :move-out fold (the default, e.g. a folded defun): next-line
;; from the fold header lands on the first visible line past the hidden lines, not inside them.
(deftest next-line-skips-move-out-fold
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer
                                  (lines "AAAA" "BBBB" "CCCC" "DDDD" "EEEE" "FFFF")))
      (lem-core::set-window-buffer buffer (current-window))
      (place-region-placeholder-overlay (point-at-line buffer 1)
                                        (point-at-line buffer 4))
      (let ((point (buffer-point buffer)))
        (move-point point (point-at-line buffer 1))
        (next-line-command)
        (ok (null (invisible-overlay-covering point)) "not left on a hidden line")
        (ok (= (position-at-point point)
               (position-at-point (point-at-line buffer 4)))
            "next-line landed on the first visible line past the fold")))))

;; vertical motion enters a :reveal fold: next-line from the header lands on the first hidden line
;; and the post-command cursor-enter hook reveals it.
(deftest next-line-enters-and-reveals-reveal-fold
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer
                                  (lines "AAAA" "BBBB" "CCCC" "DDDD" "EEEE" "FFFF")))
      (lem-core::set-window-buffer buffer (current-window))
      (let ((overlay (place-region-placeholder-overlay (point-at-line buffer 1)
                                                       (point-at-line buffer 4)
                                                       :cursor-behavior :reveal))
            (point (buffer-point buffer)))
        (ok (eq t (overlay-get overlay :invisible)) "starts hidden")
        (move-point point (point-at-line buffer 1))
        (next-line-command)
        (ok (null (overlay-get overlay :invisible)) "revealed after entering on next-line")
        (ok (= (position-at-point point)
               (position-at-point (point-at-line buffer 2)))
            "cursor landed inside the fold on the first hidden line")))))
;; a window may begin a wrapped line in its middle: the charpos of its view point counts the
;; characters of the buffer, which an invisible overlay before it has taken out of the string
(deftest logical-line-from-the-middle-of-a-line-with-hidden-text
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer (lines "abcdefghij")))
      (with-point ((start (buffer-start-point buffer))
                   (end (buffer-start-point buffer)))
        (character-offset end 4)
        (overlay-put (make-overlay start end (make-attribute)) :invisible t))
      (flet ((shown-from (charpos)
               (with-point ((point (buffer-start-point buffer)))
                 (character-offset point charpos)
                 (lem-core::logical-line-string
                  (lem-core::create-logical-line
                   point
                   (buffer-overlays buffer)
                   (lem-core::get-active-modes-class-instance buffer))))))
        (ok (equal "ij" (shown-from 8)) "after the hidden text, from the same character")
        (ok (equal "efghij" (shown-from 2)) "in the hidden text, from its end")
        (ok (equal "" (shown-from 10)) "at the end of the line")))))
