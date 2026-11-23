(defpackage :lem-darwin/scroll
  (:use :cl)
  #+(and darwin arm64)
  (:import-from :cl-objc
                :invoke)
  (:export :current-scroll-precision))
(in-package :lem-darwin/scroll)

;;; Whether a scroll comes from a trackpad or a mouse wheel.
;;;
;;; A trackpad, and the momentum macOS adds after the fingers lift, report
;;; precise deltas in points; a mouse wheel reports lines.  SDL passes both
;;; on as plain numbers, and a trackpad's can be whole numbers, so the
;;; frontend asks the NSEvent being handled.  cl-objc supports macOS on arm64
;;; only; elsewhere the answer is unknown.

(defconstant +ns-event-type-scroll-wheel+ 22
  "NSEventTypeScrollWheel.")

(defun current-scroll-precision ()
  "Return :PRECISE if the event the application is handling now is a scroll
with precise deltas (a trackpad, or its momentum), :LINES if it is a scroll
by lines (a mouse wheel), or NIL if that cannot be told: no scroll event is
being handled, or this is not macOS on arm64.  Must be called on the main
thread while the event is handled, e.g. from an SDL event watch."
  #+(and darwin arm64)
  (handler-case
      (let ((event (invoke (invoke 'ns-application shared-application) current-event)))
        (when (and (not (objc-cffi:objc-nil-object-p event))
                   (eql +ns-event-type-scroll-wheel+ (invoke event type)))
          (if (invoke event has-precise-scrolling-deltas) :precise :lines)))
    (error () nil))
  #-(and darwin arm64)
  nil)
