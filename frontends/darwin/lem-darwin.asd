(defsystem "lem-darwin"
  :description "macOS (Cocoa) window integration shared by Lem's GUI frontends."
  :depends-on ("cffi"
               "log4cl"
               (:feature (:and :darwin :arm64) "cl-objc"))
  :components ((:file "appearance")
               (:file "scroll")))
