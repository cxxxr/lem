(defpackage :lem-sdl3/deploy
  (:use :cl))
(in-package :lem-sdl3/deploy)

;;; Deploy-library settings for the packaged macOS Lem.app built from the
;;; lem-sdl3-app system (scripts/macos-deploy.bash sdl3).  Loaded only by that
;;; system, so running the SDL3 frontend from source is unaffected.

(defparameter *app-resource-directory* "../Resources/sdl3/"
  "Where the frontend's resources/ tree sits relative to the executable in
Lem.app/Contents/MacOS.  The deploy script copies it to Contents/Resources:
codesign treats every file under Contents/MacOS as code and rejects fonts
and images there.")

;; Point the frontend at the copied resources before it opens any font.
(deploy:define-hook (:boot lem-sdl3-resources) ()
  (setf lem-sdl3/resource:*resource-directory*
        (merge-pathnames *app-resource-directory* (deploy:runtime-directory))))

;; lem-sdl3.asd stands libSDL3 in for a missing libSDL3_mixer, and Lem plays
;; no audio, so neither bundle the mixer nor open it at boot: either would
;; ship or load libSDL3 a second time.
(unless (deploy:deployed-p)
  (let* ((package (find-package :sdl3-mixer))
         (symbol (and package (find-symbol "LIBSDL3-MIXER" package))))
    (when (and symbol (find symbol (cffi:list-foreign-libraries)
                            :key #'cffi:foreign-library-name))
      (let ((library (deploy:ensure-library symbol)))
        (setf (deploy:library-dont-deploy-p library) t
              (deploy:library-dont-open-p library) t)))))
