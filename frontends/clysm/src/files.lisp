;;;; files.lisp — what Lem asks the file system for in SBCL's own way

(in-package :lem-clysm)

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

;;; file-utils.lisp signals an error on implementations other than SBCL;
;;; the modification time is a Unix time, as with sb-posix:stat-mtime.
(defun lem/buffer/file-utils::file-mtime (pathname)
  "Return the file's last data modification time."
  (lem/buffer/file-utils::%call-virtual-handlers
      lem/buffer/file-utils::*virtual-file-metadata-functions*
      (list pathname :mtime)
    (lambda ()
      (let ((date (file-write-date pathname)))
        (and date (- date +unix-epoch+))))))

;;; The encoding of a file is detected by inquisitor, which names it for
;;; the implementations it knows; Clysm reads and writes UTF-8.
(setf lem/buffer/file:*external-format-function*
      (lambda (pathname)
        (multiple-value-bind (format end-of-line)
            (ignore-errors (lem-core::detect-external-format-from-file pathname))
          (values (if (keywordp format) format :utf-8)
                  (or end-of-line :lf)))))
