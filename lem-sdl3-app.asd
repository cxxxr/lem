;;; The packaged macOS Lem.app with the SDL3 frontend, built by
;;; `scripts/macos-deploy.bash sdl3'.  The default `lem' system (lem.asd)
;;; packages the webview frontend instead.
;;;
;;; The system name and :build-pathname must be the same string: deploy's
;;; osx-app-deploy-op writes Info.plist under <component-name>.app and the
;;; executable under <build-pathname>.app.

(defsystem "lem-sdl3-app"
  ;; Keep in step with the "lem" system in lem.asd.
  :version "2.3.0"
  :description "Lem with the SDL3 frontend, packaged as a macOS application."
  :defsystem-depends-on ("deploy")
  :build-operation "osx-app-deploy-op"
  :build-pathname "lem-sdl3-app"
  :entry-point "lem:main"
  :depends-on ("lem-sdl3")
  :components ((:file "src/macosx")
               (:file "frontends/sdl3/deploy")))
