(defpackage :lem-sdl3/tests/text-shaping
  (:use :cl :rove)
  (:local-nicknames (:text-shaping :lem-sdl3/text-shaping)
                    (:hb :lem-sdl3/ffi-harfbuzz)))
(in-package :lem-sdl3/tests/text-shaping)

(defun count-config-reads (function)
  "Call FUNCTION and return how many times it read :sdl3-font-features with
lem:config, which reads the settings file on every call."
  (let ((reads 0))
    (sb-int:encapsulate 'lem:config 'count-reads
                        (lambda (config key &rest rest)
                          (when (eq key :sdl3-font-features)
                            (incf reads))
                          (apply config key rest)))
    (unwind-protect (funcall function)
      (sb-int:unencapsulate 'lem:config 'count-reads))
    reads))

(deftest font-features-read-once
  ;; A shaper with no atlas: the features don't need one.
  (if (hb:available-p)
      (let ((shaper (text-shaping::%make-text-shaper nil)))
        (unwind-protect
             (progn
               (ok (= 1 (count-config-reads
                         (lambda () (dotimes (i 3) (text-shaping::features shaper)))))
                   "read once across runs")
               (ok (= 1 (count-config-reads
                         (lambda ()
                           (text-shaping:clear-shaping shaper)
                           (text-shaping::features shaper)
                           (text-shaping::features shaper))))
                   "read again after a font change"))
          (text-shaping:clear-shaping shaper)))
      (skip "HarfBuzz is not installed")))
