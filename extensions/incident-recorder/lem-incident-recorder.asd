(defsystem "lem-incident-recorder"
  :description "Records the errors that reach Lem's handlers as files for later triage."
  :depends-on ("lem/core" "yason" "cl-ppcre")
  :serial t
  :components ((:file "incident-recorder")))
