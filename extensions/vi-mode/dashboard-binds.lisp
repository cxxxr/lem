(defpackage :lem-vi-mode/dashboard-binds
  (:use :cl)
  (:import-from :lem-vi-mode/core
                :mode-specific-keymaps)
  (:import-from :lem-dashboard
                :dashboard-mode
                :*dashboard-mode-keymap*))
(in-package :lem-vi-mode/dashboard-binds)

(defmethod mode-specific-keymaps ((mode dashboard-mode))
  (list *dashboard-mode-keymap*))
