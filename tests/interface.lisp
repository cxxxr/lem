(defpackage :lem-tests/interface
  (:use :cl :rove :lem))
(in-package :lem-tests/interface)

(deftest set-frame-color-default-is-noop
  (lem-fake-interface:with-fake-interface ()
    (ok (null (set-frame-color :dark))
        "set-frame-color returns NIL on frontends that don't implement it")
    (ok (null (set-frame-color :light))
        "set-frame-color returns NIL for :light as well")))

(deftest nested-with-implementation-gives-threads-the-inner-one
  ;; *implementation* is internal: implementation reads it, but the entry of
  ;; bt2:*default-special-bindings* is what Clysm's bordeaux-threads binds.
  (let ((outer (make-instance 'lem-fake-interface:fake-interface))
        (inner (make-instance 'lem-fake-interface:fake-interface)))
    (with-implementation outer
      (with-implementation inner
        (ok (eq inner (bt2:join-thread (bt2:make-thread #'implementation))))
        ;; (*implementation* is internal to lem-core; its entry is what the
        ;; shim binds, which (implementation) on SBCL does not show.)
        (testing "one entry, so that a shim that binds every entry agrees"
          (ok (equal (list inner)
                     (mapcar #'cdr
                             (remove 'lem-core::*implementation*
                                     bt2:*default-special-bindings*
                                     :key #'car :test-not #'eq)))))))))
