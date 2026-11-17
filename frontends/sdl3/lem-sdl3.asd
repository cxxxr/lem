;;; Workaround: cl-sdl3 (https://github.com/aiffc/cl-sdl3) unconditionally
;;; loads libSDL3_mixer from src/extr/sdl-mixer/lib.lisp, whose last form is
;;; (cffi:use-foreign-library libsdl3-mixer).  Lem does not use audio, so when
;;; that library is missing we take CFFI's USE-VALUE restart and substitute the
;;; already-loaded libSDL3, which lets the file (and ASDF's bookkeeping) finish
;;; normally.  The mixer bindings stay defined but their foreign symbols are
;;; unresolved.  Remove this once upstream makes the mixer an optional system.
(defun lem-sdl3-mixer-lib-component-p (component)
  (and (typep component 'asdf:cl-source-file)
       (equal (asdf:component-name component) "lib")
       (equal (asdf:component-name (asdf:component-parent component)) "sdl-mixer")
       (equal (asdf:component-name (asdf:component-system component)) "sdl3")))

(defmethod asdf:perform :around ((op asdf:load-op) (component asdf:cl-source-file))
  (if (lem-sdl3-mixer-lib-component-p component)
      (handler-bind
          ((error (lambda (condition)
                    (let ((restart (find-restart 'use-value condition)))
                      (when (and restart
                                 (string= (symbol-name (type-of condition))
                                          "LOAD-FOREIGN-LIBRARY-ERROR"))
                        (warn "lem-sdl3: libSDL3_mixer not found; audio bindings disabled.")
                        (invoke-restart restart
                                        '(:or (:framework "SDL3")
                                          (:default "libSDL3")
                                          "libSDL3.so.0")))))))
        (call-next-method))
      (call-next-method)))

(defsystem "lem-sdl3"
  :description "SDL3 frontend for Lem."
  :depends-on ("sdl3"
               "lem/core")
  :serial t
  ;; Files not listed yet are copied from frontends/sdl2 and still call
  ;; cl-sdl2; they are added as they are ported:
  ;;   keyboard mouse main (app and input)
  ;;   graphics image-buffer tree color-picker (widgets; need main)
  :components ((:file "ffi")
               (:file "wm")
               (:file "resource")
               (:file "platform")
               (:file "font")
               (:file "icon")
               (:file "text-surface-cache")
               (:file "log")
               (:file "sdl3")
               (:file "icon-font")
               (:file "utils")
               (:file "display")
               (:file "view")
               (:file "drawing"))
  :in-order-to ((test-op (test-op "lem-sdl3/tests"))))

(defsystem "lem-sdl3/tests"
  :depends-on ("lem-sdl3"
               "rove")
  :components ((:module "tests"
                :components ((:file "ffi")
                             (:file "font")
                             (:file "drawing"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
