;;; Build the ncurses Lem into VERIFY_BINARY, like scripts/build-ncurses.lisp.

(ql:quickload :lem-ncurses :silent t)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die (uiop:getenv "VERIFY_BINARY")
                          :toplevel #'lem:main
                          :executable t)
