(defpackage :lem-sdl3/tests/cursor-blink
  (:use :cl :rove)
  (:local-nicknames (:cursor-blink :lem-sdl3/cursor-blink)))
(in-package :lem-sdl3/tests/cursor-blink)

(deftest blink-step-alternates-phases
  (let ((cursor-blink:*cursor-blinks* nil)
        (blink (cursor-blink:make-cursor-blink)))
    (ng (cursor-blink:cursor-blink-off-p blink))
    (ok (cursor-blink:blink-step blink))
    (ok (cursor-blink:cursor-blink-off-p blink))
    (ok (cursor-blink:blink-step blink))
    (ng (cursor-blink:cursor-blink-off-p blink))
    (ok (= 1 (cursor-blink:cursor-blink-count blink)))))

(deftest blink-step-stops-shown-after-the-last-blink
  (let ((cursor-blink:*cursor-blinks* 2)
        (blink (cursor-blink:make-cursor-blink)))
    (testing "the first blink goes on"
      (ok (cursor-blink:blink-step blink))
      (ok (cursor-blink:blink-step blink)))
    (testing "the second blink ends with the cursor shown"
      (ok (cursor-blink:blink-step blink))
      (ng (cursor-blink:blink-step blink))
      (ng (cursor-blink:cursor-blink-off-p blink)))))

(deftest stop-cursor-blink-shows-the-cursor
  (let ((blink (cursor-blink:make-cursor-blink)))
    (ng (cursor-blink:stop-cursor-blink blink))
    (cursor-blink:blink-step blink)
    (testing "it reports that the cursor was hidden"
      (ok (cursor-blink:stop-cursor-blink blink)))
    (ng (cursor-blink:cursor-blink-off-p blink))
    (ok (= 0 (cursor-blink:cursor-blink-count blink)))))
