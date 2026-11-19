;;;; browser.lisp — the files of the computer, for Lem in a browser
;;;;
;;;; In a browser Lem's files are in memory (host/web/vfs.mjs; those written
;;;; in the home directory are kept by the browser).  A file of the computer
;;;; is brought in with M-x open-local-file (the browser's file chooser), and
;;;; a buffer is saved to the computer with M-x download-file (a download).

(in-package :lem-clysm)

(sys::%defvar-host *choose-local-file*
  (js:eval "(done) => { const input = document.createElement('input');
     input.type = 'file';
     input.addEventListener('change', () => {
       const file = input.files[0];
       if (file) file.text().then((text) => done(file.name, text)); });
     input.click(); }"))

(sys::%defvar-host *download*
  (js:eval "(name, text) => { const a = document.createElement('a');
     a.href = URL.createObjectURL(new Blob([text], { type: 'text/plain' }));
     a.download = name;
     a.click();
     setTimeout(() => URL.revokeObjectURL(a.href), 10000); }"))

(defun open-copied-file (name text)
  (let ((path (merge-pathnames (file-namestring name) (user-homedir-pathname))))
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    (lem:find-file path)
    (lem:message "Copied ~A to ~A" name (namestring path))
    (lem:redraw-display)))

(lem:define-command open-local-file () ()
  "Choose a file of the computer and open a copy of it in the home directory."
  (js:funcall *choose-local-file*
              (lambda (name text)
                (lem:send-event (lambda () (open-copied-file name text)))
                nil)))

(lem:define-command download-file () ()
  "Save the text of the buffer to a file of the computer (the browser downloads it)."
  (let* ((buffer (lem:current-buffer))
         (file (lem:buffer-filename buffer))
         (name (if file (file-namestring file) (lem:buffer-name buffer))))
    (js:funcall *download* name (lem:buffer-text buffer))
    (lem:message "Downloading ~A" name)))
