(in-package :lem)

(export '(unmark-buffer
          toggle-read-only
          bolp
          eolp
          bobp
          eobp
          insert-char
          insert-lines
          insert-string
          insert-newline
          newline-and-indent
          open-line
          delete-char
          backward-delete-char
          kill-line
          goto-column
          beginning-of-line
          end-of-line
          goto-line
          beginning-of-buffer
          end-of-buffer
          next-line
          prev-line
          next-char
          prev-char
          next-page
          prev-page
          mark-set
          exchange-point-mark
          following-char
          preceding-char
          char-after
          char-before
          replace-char
          entab-line
          detab-line
          blank-line-p
          delete-blank-lines
          transpose-characters
          erase-buffer
          delete-while-whitespaces
          skip-chars-forward
          skip-chars-backward
          just-one-space
          delete-indentation
          back-to-indentation
          undo
          redo))

(define-key *global-keymap* (kbd "M-~") 'unmark-buffer)
(define-command unmark-buffer () ()
  (setf (buffer-modified-p (window-buffer)) nil)
  t)

(define-key *global-keymap* (kbd "C-x C-q") 'toggle-read-only)
(define-command toggle-read-only () ()
  (setf (buffer-read-only-p (window-buffer))
        (not (buffer-read-only-p (window-buffer))))
  t)

(define-command rename-buffer (name) ("sRename buffer: ")
  (setf (buffer-name (current-buffer)) name)
  t)

(defun head-line-p (window linum)
  (declare (ignore window))
  (values (<= linum 1) (- 2 linum)))

(defun tail-line-p (window linum)
  (let ((nlines (buffer-nlines (window-buffer window))))
    (values (<= nlines linum) (- nlines linum))))

(defun bolp ()
  (zerop (window-cur-col)))

(defun eolp ()
  (= (window-cur-col)
     (buffer-line-length
      (window-buffer)
      (window-cur-linum))))

(defun bobp ()
  (and (head-line-p *current-window* (window-cur-linum))
       (bolp)))

(defun eobp ()
  (and (tail-line-p
        *current-window*
        (window-cur-linum))
       (eolp)))

(defun insert-char (c n)
  (dotimes (_ n t)
    (when (buffer-insert-char
           (window-buffer)
           (window-cur-linum)
           (window-cur-col)
           c)
      (next-char 1))))

(define-key *global-keymap* (kbd "C-q") 'quoted-insert)
(define-command quoted-insert (&optional (n 1)) ("p")
  (let ((c (getch)))
    (dotimes (_ n t)
      (cond ((char= c key::ctrl-m)
             (insert-newline 1))
            ((char= c key::ctrl-d)
             (delete-char))
            (t
             (insert-char c 1))))))

(defun insert-lines (lines)
  (do ((rest lines (cdr rest)))
      ((null rest))
    (buffer-insert-line
     (window-buffer)
     (window-cur-linum)
     (window-cur-col)
     (car rest))
    (next-char (length (car rest)))
    (when (cdr rest)
      (insert-newline 1))))

(define-command insert-string (str) ("sInsert string: ")
  (insert-lines (split-string str #\newline)))

(define-key *global-keymap* (kbd "C-m") 'insert-newline)
(define-command insert-newline (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (buffer-insert-newline (window-buffer)
                           (window-cur-linum)
                           (window-cur-col))
    (next-line 1)))

(define-key *global-keymap* (kbd "C-j") 'newline-and-indent)
(define-command newline-and-indent (n) ("p")
  (dotimes (_ n t)
    (let ((spaces (second
                   (multiple-value-list
                    (count-indent)))))
      (unless (and (insert-newline 1)
                   (insert-string spaces))
        (return nil)))))

(define-key *global-keymap* (kbd "C-o") 'open-line)
(define-command open-line (n) ("p")
  (insert-newline n)
  (prev-char n))

(define-key *global-keymap* (kbd "C-d") 'delete-char)
(define-key *global-keymap* (kbd "dc") 'delete-char)
(define-command delete-char (&optional n does-not-kill-p) ("P")
  (cond
   ((and n (minusp n))
    (backward-delete-char (- n)))
   (t
    (multiple-value-bind (result lines)
        (buffer-delete-char
         (window-buffer)
         (window-cur-linum)
         (window-cur-col)
         (or n 1))
      (when result
        (when (and (not does-not-kill-p) n)
          (with-kill ()
            (kill-push lines)))
        (dolist (win *window-list*)
          (when (and (not (eq win *current-window*))
                     (eq (window-buffer win) (window-buffer))
                     (> (window-cur-linum win)
                        (buffer-nlines (window-buffer win))))
            (decf (window-cur-linum win)
                  (- (window-cur-linum win)
                     (buffer-nlines (window-buffer win))))))
        result)))))

(define-key *global-keymap* (kbd "C-h") 'backward-delete-char)
(define-key *global-keymap* (kbd "backspace") 'backward-delete-char)
(define-command backward-delete-char (&optional n does-not-kill-p) ("P")
  (cond ((null n)
         (when (prev-char)
           (delete-char)))
        ((minusp n)
         (delete-char (- n) does-not-kill-p))
        (t
         (when (prev-char n)
           (delete-char n does-not-kill-p)))))

(define-key *global-keymap* (kbd "C-k") 'kill-line)
(define-command kill-line (&optional n) ("P")
  (cond
   ((null n)
    (let ((size (- (buffer-line-length
                    (window-buffer)
                    (window-cur-linum))
                   (window-cur-col))))
      (if (zerop size)
          (delete-char 1)
          (delete-char size))))
   ((plusp n)
    (dotimes (_ n)
      (kill-line)))))

(defun goto-column (col)
  (setf (window-cur-col) col)
  (setf (window-max-col) col))

(define-key *global-keymap* (kbd "C-a") 'beginning-of-line)
(define-command beginning-of-line () ()
  (goto-column 0)
  t)

(define-key *global-keymap* (kbd "C-e") 'end-of-line)
(define-command end-of-line () ()
  (goto-column (buffer-line-length
                (window-buffer)
                (window-cur-linum)))
  t)

(define-key *global-keymap* (kbd "M-g") 'goto-line)
(define-command goto-line (n &optional does-not-recenter-p) ("P")
  (unless n
    (setq n (minibuf-read-number "Line to GOTO: ")))
  (when (< 0 n (1+ (buffer-nlines (window-buffer))))
    (setf (window-cur-linum) n)
    (beginning-of-line)
    (unless does-not-recenter-p
      (recenter))
    t))

(define-key *global-keymap* (kbd "M-<") 'beginning-of-buffer)
(define-command beginning-of-buffer () ()
  (goto-line 1)
  (goto-column 0)
  t)

(define-key *global-keymap* (kbd "M->") 'end-of-buffer)
(define-command end-of-buffer () ()
  (goto-line (buffer-nlines (window-buffer)) t)
  (goto-column (buffer-line-length
                (window-buffer)
                (window-cur-linum)))
  t)

(defun %buffer-adjust-col (arg)
  (if arg
      (beginning-of-line)
      (setf (window-cur-col)
            (min (window-max-col)
                 (buffer-line-length
                  (window-buffer)
                  (window-cur-linum))))))

(define-key *global-keymap* (kbd "C-n") 'next-line)
(define-key *global-keymap* (kbd "down") 'next-line)
(define-command next-line (&optional n) ("P")
  (if (and n (minusp n))
      (prev-line (- n))
      (if (dotimes (_ (or n 1) t)
            (if (tail-line-p *current-window* (window-cur-linum))
                (return nil)
                (incf (window-cur-linum))))
          (progn (%buffer-adjust-col n) t)
          (progn (end-of-line) nil))))

(define-key *global-keymap* (kbd "C-p") 'prev-line)
(define-key *global-keymap* (kbd "up") 'prev-line)
(define-command prev-line (&optional n) ("P")
  (if (and n (minusp n))
      (next-line (- n))
      (if (dotimes (_ (or n 1) t)
            (if (head-line-p *current-window* (window-cur-linum))
                (return)
                (decf (window-cur-linum))))
          (progn (%buffer-adjust-col n) t)
          (progn (beginning-of-line) nil))))

(define-key *global-keymap* (kbd "C-f") 'next-char)
(define-key *global-keymap* (kbd "right") 'next-char)
(define-command next-char (&optional (n 1)) ("p")
  (if (minusp n)
      (prev-char (- n))
      (dotimes (_ n t)
        (cond
         ((eobp)
          (return nil))
         ((eolp)
          (next-line 1))
         (t
          (goto-column (1+ (window-cur-col))))))))

(define-key *global-keymap* (kbd "C-b") 'prev-char)
(define-key *global-keymap* (kbd "left") 'prev-char)
(define-command prev-char (&optional (n 1)) ("p")
  (if (minusp n)
      (next-char (- n))
      (dotimes (_ n t)
        (cond
         ((bobp)
          (return nil))
         ((bolp)
          (prev-line 1)
          (end-of-line))
         (t
          (goto-column (1- (window-cur-col))))))))

(define-key *global-keymap* (kbd "C-v") 'next-page)
(define-command next-page (&optional (n 1)) ("p")
  (scroll-down (* n (- (window-nlines) 1))))

(define-key *global-keymap* (kbd "M-v") 'prev-page)
(define-command prev-page (&optional (n 1)) ("p")
  (scroll-up (* n (- (window-nlines) 1))))

(define-key *global-keymap* (kbd "C-x ]") 'next-page-char)
(define-command next-page-char (&optional (n 1)) ("p")
  (or (search-forward (string #\page))
      (end-of-buffer)))

(define-key *global-keymap* (kbd "C-x [") 'prev-page-char)
(define-command prev-page-char (&optional (n 1)) ("p")
  (or (search-backward (string #\page))
      (beginning-of-buffer)))

(define-key *global-keymap* (kbd "C-@") 'mark-set)
(define-command mark-set () ()
  (let ((buffer (window-buffer)))
    (setf (buffer-mark-linum buffer)
          (window-cur-linum))
    (setf (buffer-mark-col buffer)
          (window-cur-col))
    (minibuf-print "Mark set")
    t))

(define-key *global-keymap* (kbd "C-x C-x") 'exchange-point-mark)
(define-command exchange-point-mark () ()
  (let ((buffer (window-buffer)))
    (when (buffer-check-marked buffer)
      (psetf
       (window-cur-linum) (buffer-mark-linum buffer)
       (window-cur-col) (buffer-mark-col buffer)
       (buffer-mark-linum buffer) (window-cur-linum)
       (buffer-mark-col buffer) (window-cur-col))
      (setf (window-max-col) (buffer-mark-col buffer))
      t)))

(defun following-char ()
  (buffer-get-char (window-buffer)
                   (window-cur-linum)
                   (window-cur-col)))

(defun preceding-char ()
  (cond
   ((bobp)
    nil)
   ((bolp)
    (buffer-get-char (window-buffer)
                     (1- (window-cur-linum))
                     (buffer-line-length (window-buffer)
                                         (1- (window-cur-linum)))))
   (t
    (buffer-get-char (window-buffer)
                     (window-cur-linum)
                     (1- (window-cur-col))))))

(defun char-after (&optional (n 0))
  (if (zerop n)
      (following-char)
      (let ((point (point)))
        (if (next-char n)
            (prog1 (following-char)
              (prev-char n))
            (progn
              (point-set point)
              nil)))))

(defun char-before (&optional (n 1))
  (if (= n 1)
      (preceding-char)
      (let ((point (point)))
        (if (prev-char (1- n))
            (prog1 (preceding-char)
              (next-char (1- n)))
            (progn
              (point-set point)
              nil)))))

(defun replace-char (c)
  (delete-char)
  (buffer-insert-char
   (window-buffer)
   (window-cur-linum)
   (window-cur-col)
   c))

(defun count-indent ()
  (save-excursion
   (beginning-of-line)
   (let ((count 0)
         (chars))
     (do () ((eolp))
       (let ((c (following-char)))
         (case c
           (#\space
            (incf count))
           (#\tab
            (setq count (char-width #\tab count)))
           (otherwise
            (return)))
         (push c chars)
         (next-char 1)))
     (values count
             (coerce (nreverse chars) 'string)))))

(defun tab-line-aux (n make-space-str)
  (dotimes (_ n t)
    (let ((count (count-indent)))
      (multiple-value-bind (div mod)
          (floor count *tab-size*)
        (beginning-of-line)
        (delete-while-whitespaces t nil)
        (insert-string (funcall make-space-str div))
        (insert-char #\space mod)))
    (unless (next-line 1)
      (return))))

(define-key *global-keymap* (kbd "C-x C-e") 'entab-line)
(define-command entab-line (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string n :initial-element #\tab))))

(define-key *global-keymap* (kbd "C-x C-a") 'detab-line)
(define-command detab-line (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string (* n *tab-size*) :initial-element #\space))))

(defun blank-line-p ()
  (save-excursion
   (beginning-of-line)
   (do ((count 0 (1+ count)))
       ((eolp) (1+ count))
     (case (following-char)
       ((#\space #\tab))
       (otherwise (return nil)))
     (next-char))))

(define-key *global-keymap* (kbd "C-x C-o") 'delete-blank-lines)
(define-command delete-blank-lines () ()
  (do ()
      ((not (blank-line-p))
       (next-line 1))
    (unless (prev-line 1)
      (return)))
  (do () ((eobp))
    (let ((result (blank-line-p)))
      (unless (and result (delete-char result t))
        (return)))))

(define-key *global-keymap* (kbd "C-t") 'transpose-characters)
(define-command transpose-characters () ()
  (cond ((bolp))
        ((eolp)
         (let* ((c1 (char-before 1))
                (c2 (char-before 2)))
           (unless (eql c2 #\newline)
             (backward-delete-char)
             (backward-delete-char)
             (insert-char c1 1)
             (insert-char c2 1))))
        (t
         (let* ((c1 (following-char))
                (c2 (preceding-char)))
           (delete-char)
           (backward-delete-char)
           (insert-char c1 1)
           (insert-char c2 1)))))

(define-command erase-buffer () ()
  (beginning-of-buffer)
  (buffer-erase (window-buffer))
  (beginning-of-buffer)
  t)

(defun delete-while-whitespaces (&optional ignore-newline-p use-kill-ring)
  (do ((n 0 (1+ n))) ((eobp))
    (let ((c (following-char)))
      (if (or (and ignore-newline-p (char= c #\newline))
              (not (syntax-space-char-p c)))
          (return n)
          (delete-char 1 (not use-kill-ring))))))

(macrolet ((def (name at-char step-char)
                `(defun ,name (pred &optional not-p)
                   (do ()
                       ((not (if (funcall pred (,at-char))
                                 (not not-p)
                                 not-p))
                        t)
                     (unless (,step-char)
                       (return))))))
  (def skip-chars-forward following-char next-char)
  (def skip-chars-backward preceding-char prev-char))

(define-key *global-keymap* (kbd "M-Space") 'just-one-space)
(define-command just-one-space () ()
  (skip-chars-backward 'syntax-space-char-p)
  (delete-while-whitespaces nil nil)
  (insert-char #\space 1)
  t)

(define-key *global-keymap* (kbd "M-^") 'delete-indentation)
(define-command delete-indentation () ()
  (beginning-of-line)
  (let ((point (point)))
    (prev-line)
    (end-of-line)
    (delete-char (region-count (point) point) t)
    (just-one-space)
    t))

(defun insert-paren-hilighting-aux (c n)
  (when (insert-char c n)
    (or (macro-running-p)
        (save-excursion
         (when (backward-list 1)
           (window-update-all)
           (cl-charms/low-level:timeout 1000)
           (let ((c (cl-charms/low-level:getch)))
             (unless (= -1 c)
               (ungetch (code-char c))))
           (cl-charms/low-level:timeout -1)
           t)))))

(macrolet ((def (name c)
                `(progn
                   (define-key *global-keymap* (kbd ,(string c))
                     ',name)
                   (define-command ,name (n) ("p")
                     (insert-paren-hilighting-aux ,c n)))))
  (def insert-paren-hilighting #\))
  (def insert-brace-hilighting #\])
  (def insert-block-hilighting #\}))

(define-key *global-keymap* (kbd "M-m") 'back-to-indentation)
(define-command back-to-indentation () ()
  (beginning-of-line)
  (skip-chars-forward #'(lambda (c) (member c '(#\space #\tab))))
  t)

(define-key *global-keymap* (kbd "C-z") 'undo)
(define-command undo () ()
  (let ((point (buffer-undo (window-buffer))))
    (when point
      (point-set point)
      t)))

(define-key *global-keymap* (kbd "M-C-z") 'redo)
(define-command redo () ()
  (let ((point (buffer-redo (window-buffer))))
    (when point
      (point-set point)
      t)))
