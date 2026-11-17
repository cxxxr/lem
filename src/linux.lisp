(in-package :lem-core)

;; cl-charms lists the unversioned "libncursesw.so" first. On Debian and
;; Ubuntu that file is an ld script ("INPUT(libncursesw.so.6 -ltinfo)"), and
;; deploy only follows "GROUP (" scripts, so it ends up with no library path
;; and aborts the build. Point deploy at the real shared object that CFFI
;; actually opened instead. Only relevant while building.
(defun find-system-library-file (name)
  "Return the first file called NAME in deploy's system library directories."
  (loop :for directory :in deploy:*system-source-directories*
        :for file := (first (directory (merge-pathnames name directory)))
        :when file
          :return file))

(unless (deploy:deployed-p)
  (let* ((package (find-package :cl-charms/low-level))
         (symbol (and package (find-symbol "LIBCURSES" package))))
    (when (and symbol (find symbol (cffi:list-foreign-libraries)
                            :key #'cffi:foreign-library-name))
      (let* ((library (deploy:ensure-library symbol))
             (path (deploy:library-path library)))
        (unless (and path (uiop:file-exists-p path))
          (let ((file (find-system-library-file
                       (cffi:foreign-library-pathname library))))
            (when file
              (setf (deploy:library-path library) file))))))))
