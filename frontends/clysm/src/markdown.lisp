;;;; markdown.lisp — Markdown shown as it reads, while it is edited
;;;;
;;;; In a Markdown buffer, markdown-live-mode (on by default) draws the text
;;;; as a rendered document with overlays on the text, which stays as it is:
;;;; headings larger (text of other sizes, display.lisp), emphasis, code,
;;;; links, quotes, lists and tasks, rules, tables, code blocks on a
;;;; background, and images in place of ![alt](file).  The marks of Markdown
;;;; (# ** ` [ ]( ) ...) are hidden, except on the line of the cursor, where
;;;; they are shown dim to be edited (as in Obsidian's live preview).  The
;;;; overlays are made again after a command that changed the text, moved
;;;; the cursor to another line or scrolled, for the lines windows show.
;;;;
;;;; Sections under headings and code blocks fold (Lem's folds: overlays
;;;; that hide lines): Tab on a heading folds or unfolds its section, and
;;;; Shift-Tab cycles the buffer through its outline, its headings and all.

(in-package :lem-clysm)

;;; ---- colours and attributes, from the theme

(defun color-of (thing)
  (cond ((null thing) nil)
        ((typep thing 'lem:color) thing)
        ((stringp thing) (lem:parse-color thing))))

(defun mix-colors (a b ratio)
  "The colour RATIO of the way from A to B (colours or their names), as #rrggbb."
  (let ((a (color-of a)) (b (color-of b)))
    (cond ((and a b)
           (flet ((mix (x y) (round (+ (* x (- 1 ratio)) (* y ratio)))))
             (lem:color-to-hex-string
              (lem:make-color (mix (lem:color-red a) (lem:color-red b))
                              (mix (lem:color-green a) (lem:color-green b))
                              (mix (lem:color-blue a) (lem:color-blue b))))))
          (b (lem:color-to-hex-string b))
          (a (lem:color-to-hex-string a)))))

(defun theme-foreground (name)
  "The foreground of the attribute NAME of the theme, or NIL."
  (let ((attribute (lem:ensure-attribute name nil)))
    (and attribute (lem:attribute-foreground attribute))))

(defvar *markdown-attributes* nil
  "(colours of the theme . plist of the attributes made for them)")

(defun make-markdown-attributes (background foreground)
  (let ((muted (mix-colors background foreground 0.6))
        (dim (mix-colors background foreground 0.35))
        (accent (or (theme-foreground 'lem:syntax-function-name-attribute) foreground)))
    (flet ((heading (scale name)
             (lem:make-attribute :foreground (or (theme-foreground name) foreground)
                                 :bold t :plist (list :scale scale))))
      (list :h1 (heading 2 'lem:syntax-keyword-attribute)
            :h2 (heading 1.6 'lem:syntax-function-name-attribute)
            :h3 (heading 1.35 'lem:syntax-type-attribute)
            :h4 (heading 1.2 'lem:syntax-variable-attribute)
            :h5 (heading 1.1 'lem:syntax-constant-attribute)
            :h6 (heading 1 'lem:syntax-builtin-attribute)
            :mark (lem:make-attribute :foreground dim)
            :bold (lem:make-attribute :bold t)
            :italic (lem:make-attribute :italic t)
            :strike (lem:make-attribute :strikethrough t :foreground muted)
            :code (lem:make-attribute
                   :foreground (or (theme-foreground 'lem:syntax-string-attribute) foreground)
                   :background (mix-colors background foreground 0.12))
            :code-block (lem:make-attribute :background (mix-colors background foreground 0.06))
            :fence (lem:make-attribute :foreground dim :plist (list :scale 0.85))
            :link (lem:make-attribute :foreground accent :underline t)
            :quote (lem:make-attribute :foreground muted :italic t)
            :quote-bar (lem:make-attribute :foreground accent :bold t)
            :bullet (lem:make-attribute :foreground accent :bold t)
            :done (lem:make-attribute :foreground muted :strikethrough t)
            :rule (lem:make-attribute :foreground dim)
            :table (lem:make-attribute :foreground dim)
            :missing (lem:make-attribute :foreground muted :italic t)))))

(defun theme-key ()
  "The colours of the theme: the attributes are made again when they change."
  (let ((implementation (lem:implementation)))
    (list (mix-colors nil (lem-if:get-background-color implementation) 1)
          (mix-colors nil (lem-if:get-foreground-color implementation) 1))))

(defun md-attribute (name)
  "The attribute NAME of the rendered Markdown, for the colours of the theme."
  (let ((key (theme-key)))
    (unless (equal key (car *markdown-attributes*))
      (setf *markdown-attributes* (cons key (apply #'make-markdown-attributes key))))
    (getf (cdr *markdown-attributes*) name)))

;;; ---- images of the documents

(defvar *markdown-images* (make-hash-table :test 'equal)
  "source -> (:loaded image), :loading or (:failed message)")

(defun url-p (string)
  (ppcre:scan "^(?i)(https?|data|blob):" string))

(defun image-source (target buffer)
  "The URL or the file of the image TARGET of a Markdown link in BUFFER."
  (let ((target (string-trim "<>" target)))
    (cond ((url-p target) target)
          ((and (plusp (length target)) (char= (char target 0) #\~))
           (namestring (merge-pathnames (string-left-trim "~/" target) (user-homedir-pathname))))
          (t (namestring (merge-pathnames target (or (lem:buffer-directory buffer)
                                                     (user-homedir-pathname))))))))

(defun markdown-image (source)
  "The image of SOURCE if it is loaded; otherwise :LOADING (it is being
loaded: the buffers are drawn again when it is) or (:FAILED message)."
  (let ((state (gethash source *markdown-images*)))
    (cond ((and (consp state) (eq (first state) :loaded)) (second state))
          (state state)
          (t
           (setf (gethash source *markdown-images*) :loading)
           (flet ((done (image &optional error)
                    (setf (gethash source *markdown-images*)
                          (if image (list :loaded image) (list :failed error)))
                    (refresh-markdown-buffers :force t)))
             (cond ((url-p source) (load-image-url source #'done))
                   ((ignore-errors (probe-file source)) (load-image source #'done))
                   (t (setf (gethash source *markdown-images*) (list :failed "no such file")))))
           (let ((state (gethash source *markdown-images*)))
             (if (and (consp state) (eq (first state) :loaded)) (second state) state))))))

(defun image-display (source alt width)
  "What an image shows as, (string attribute): the image of SOURCE, at most
WIDTH pixels wide, or ALT while it is loaded or if it cannot be.  The second
value is its height in pixels."
  (let ((image (markdown-image source))
        (alt (if (plusp (length alt)) alt "image")))
    (cond ((typep image 'image)
           (let* ((shown (min (image-width image) (max 16 width)))
                  (height (round (* shown (image-height image)) (max 1 (image-width image)))))
             (values (list " " (image-attribute image :width shown :height height))
                     height)))
          ((eq image :loading)
           (list (format nil "[~A]" alt) (md-attribute :missing)))
          (t
           (list (format nil "[~A: ~A]" alt (if (consp image) (second image) "cannot be shown"))
                 (md-attribute :missing))))))

;;; ---- overlays
;;;
;;; A line is decorated by a DECORATOR: the point of the line, whether it is
;;; the line of the cursor (the marks are then shown), and the width of the
;;; window in pixels.

(defstruct decorator point line reveal width folded)

(defun note-rows (decorator rows)
  "Note that the line takes ROWS lines of the window (a heading, an image):
keeping the cursor in the window counts them (KEEP-CURSOR-SHOWN)."
  (let ((table (or (lem:buffer-value (lem:point-buffer (decorator-point decorator)) 'markdown-rows)
                   (setf (lem:buffer-value (lem:point-buffer (decorator-point decorator)) 'markdown-rows)
                         (make-hash-table)))))
    (setf (gethash (decorator-line decorator) table)
          (max rows (gethash (decorator-line decorator) table 1)))))

(defun column-point (decorator column)
  (let ((point (lem:copy-point (decorator-point decorator) :temporary)))
    (lem:line-start point)
    (lem:character-offset point column)
    point))

(defun remember-overlay (decorator overlay)
  (push overlay (lem:buffer-value (lem:point-buffer (decorator-point decorator))
                                  'markdown-overlays))
  overlay)

(defun add-overlay (decorator start end attribute &rest properties)
  "An overlay on the columns START..END of the line, with ATTRIBUTE (an
attribute or the name of one of md-attribute) and the overlay PROPERTIES."
  (when (< start end)
    (let ((overlay (lem:make-overlay (column-point decorator start)
                                     (column-point decorator end)
                                     (cond ((keywordp attribute) (md-attribute attribute))
                                           (attribute)
                                           (t (lem:make-attribute))))))
      (loop for (key value) on properties by #'cddr
            do (lem:overlay-put overlay key value))
      (remember-overlay decorator overlay))))

(defun add-line-background (decorator attribute)
  (remember-overlay decorator (lem:make-line-overlay (decorator-point decorator)
                                                     (md-attribute attribute))))

(defun hide (decorator start end)
  "Hide the mark START..END, or dim it on the line of the cursor."
  (if (decorator-reveal decorator)
      (add-overlay decorator start end :mark)
      (add-overlay decorator start end nil :invisible t)))

(defun show-as (decorator start end display)
  "Show START..END as DISPLAY, (string attribute), or dim it on the line of
the cursor."
  (if (decorator-reveal decorator)
      (add-overlay decorator start end :mark)
      (add-overlay decorator start end nil :display display)))

(defun clear-markdown-overlays (buffer)
  (mapc #'lem:delete-overlay (lem:buffer-value buffer 'markdown-overlays))
  (setf (lem:buffer-value buffer 'markdown-overlays) nil))

;;; ---- the text of a line: code, images, links, emphasis

(defparameter *emphasis*
  '(("(\\*\\*|__)(?=\\S)(.+?)(?<=\\S)\\1" :bold)
    ("~~(?=\\S)(.+?)(?<=\\S)~~" :strike)
    ("(?<![*\\w])\\*(?=[^\\s*])(.+?)(?<=[^\\s*])\\*(?![*\\w])" :italic)
    ("(?<![_\\w])_(?=[^\\s_])(.+?)(?<=[^\\s_])_(?![_\\w])" :italic))
  "(regex attribute): the last group of REGEX is the text, around it the marks.")

(defun decorate-text (decorator string from)
  "Decorate the text of STRING (the line) from the column FROM."
  (let ((taken '()))
    (flet ((free-p (start end)
             (notany (lambda (range) (and (< start (cdr range)) (< (car range) end))) taken))
           (take (start end) (push (cons start end) taken)))
      ;; code spans first: what is in them is not Markdown
      (ppcre:do-scans (start end rs re "(`+)(?!`)(.+?)(?<!`)\\1(?!`)" string nil :start from)
        (take start end)
        (hide decorator start (aref rs 1))
        (add-overlay decorator (aref rs 1) (aref re 1) :code)
        (hide decorator (aref re 1) end))
      ;; ![alt](image)
      (ppcre:do-scans (start end rs re "!\\[([^\\]]*)\\]\\(([^)\\s]+)(?:\\s+\"[^\"]*\")?\\)" string
                       nil :start from)
        (when (free-p start end)
          (take start end)
          (multiple-value-bind (display height)
              (image-display (image-source (subseq string (aref rs 1) (aref re 1))
                                           (lem:point-buffer (decorator-point decorator)))
                             (subseq string (aref rs 0) (aref re 0))
                             (decorator-width decorator))
            (show-as decorator start end display)
            (when (and height (not (decorator-reveal decorator)))
              (note-rows decorator (ceiling height *char-height*))))))
      ;; [text](target): the text underlined, the target hidden
      (ppcre:do-scans (start end rs re "\\[([^\\]]+)\\]\\(([^)]*)\\)" string nil :start from)
        (when (free-p start end)
          (take start end)
          (hide decorator start (aref rs 0))
          (add-overlay decorator (aref rs 0) (aref re 0) :link)
          (hide decorator (aref re 0) end)))
      ;; **bold**, ~~struck~~, *italic*
      (loop for (regex attribute) in *emphasis*
            do (ppcre:do-scans (start end rs re regex string nil :start from)
                 (when (free-p start end)
                   (let* ((text (1- (length rs)))
                          (open (aref rs text))
                          (close (aref re text)))
                     (hide decorator start open)
                     (add-overlay decorator open close attribute)
                     (hide decorator close end))))))))

;;; ---- the blocks: headings, rules, quotes, lists, tables

(defparameter *bullets* '("•" "◦" "▪" "▫")
  "The bullets of the items of a list, by how deep it is.")

(defparameter *heading-attributes* '(:h1 :h2 :h3 :h4 :h5 :h6))

(defun decorate-line (decorator string)
  "Decorate the line STRING (not in a code block)."
  (let ((length (length string)))
    (cond
      ;; # heading
      ((ppcre:scan "^#{1,6}[ \\t]" string)
       (let* ((level (position #\# string :test-not #'char=))
              (text (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) string :start level)
                        length)))
         (add-overlay decorator 0 length (nth (1- level) *heading-attributes*))
         (note-rows decorator (ceiling (or (attribute-scale (md-attribute (nth (1- level) *heading-attributes*))) 1)))
         (if (decorator-folded decorator)
             ;; a folded section: a chevron in place of the marks
             (show-as decorator 0 text (list "▸ " (md-attribute :bullet)))
             (hide decorator 0 text))
         (decorate-text decorator string text)))
      ;; --- a rule
      ((ppcre:scan "^ {0,3}([-*_])( *\\1){2,} *$" string)
       (let ((columns (max 3 (floor (decorator-width decorator) *char-width*))))
         (show-as decorator 0 length
                  (list (make-string columns :initial-element #\─) (md-attribute :rule)))))
      ;; > quote
      ((ppcre:scan "^ {0,3}>" string)
       (let ((text (nth-value 1 (ppcre:scan "^ {0,3}> ?" string))))
         (show-as decorator 0 text (list "▎ " (md-attribute :quote-bar)))
         (add-overlay decorator text length :quote)
         (decorate-text decorator string text)))
      ;; - item, 1. item, - [ ] task
      ((ppcre:scan "^\\s*([-*+]|\\d+[.)])\\s" string)
       (multiple-value-bind (start text rs re)
           (ppcre:scan "^(\\s*)([-*+]|\\d+[.)])(\\s+)(\\[([ xX])\\]\\s)?" string)
         (declare (ignore start))
         (let ((level (floor (aref re 0) 2))
               (marker (char string (aref rs 1))))
           (cond ((aref rs 3)
                  ;; a task: its box in place of the bullet
                  (let ((done (char-equal (char string (aref rs 4)) #\x)))
                    (hide decorator (aref rs 1) (aref rs 3))
                    (show-as decorator (aref rs 3) (1- (aref re 3))
                             (list (if done "☑" "☐") (md-attribute :bullet)))
                    (when done (add-overlay decorator text length :done))))
                 ((digit-char-p marker)
                  (add-overlay decorator (aref rs 1) (aref re 1) :bullet))
                 (t
                  (show-as decorator (aref rs 1) (aref re 1)
                           (list (nth (mod level (length *bullets*)) *bullets*)
                                 (md-attribute :bullet)))))
           (decorate-text decorator string text))))
      ;; | a | table |
      ((ppcre:scan "^\\s*\\|" string)
       (if (ppcre:scan "^[\\s|:-]+$" string)
           (add-overlay decorator 0 length :table)
           (progn
             (ppcre:do-matches (start end "\\|" string)
               (add-overlay decorator start end :table))
             (decorate-text decorator string 0))))
      (t (decorate-text decorator string 0)))))

;;; ---- the buffer

(defun fence-p (string)
  "The fence (``` or ~~~) that STRING opens or closes a code block with, or NIL."
  (let ((start (position #\Space string :test-not #'char=)))
    (when (and start (<= start 3) (<= (+ start 3) (length string)))
      (let ((head (subseq string start (+ start 3))))
        (and (member head '("```" "~~~") :test #'string=) head)))))

(defun buffer-windows (buffer)
  (remove buffer (lem:window-list) :key #'lem:window-buffer :test-not #'eq))

(defun shown-lines (buffer)
  "For each window of BUFFER, (first-line . lines): the first line of the
buffer it shows (with a margin above) and how many lines it can show (with
a margin below); the lines of folds are not counted."
  (mapcar (lambda (window)
            (cons (max 1 (- (lem:line-number-at-point (lem:window-view-point window)) 2))
                  (+ (lem:window-height window) 4)))
          (buffer-windows buffer)))

(defun cursor-line (buffer)
  "The line of the cursor in BUFFER, if it is the buffer of the current window."
  (when (eq buffer (lem:window-buffer (lem:current-window)))
    (lem:line-number-at-point (lem:buffer-point buffer))))

(defun hidden-line-p (point)
  "Whether the line of POINT is hidden in a fold."
  (lem:line-continuation-p point))

(defun folded-lines (buffer)
  "The lines of BUFFER that begin a fold."
  (loop for overlay in (lem:buffer-overlays buffer)
        when (lem:overlay-get overlay :fold)
          collect (lem:line-number-at-point (lem:overlay-start overlay))))

(defun decorate-buffer (buffer)
  "Make again the overlays of the lines of BUFFER that windows show."
  (clear-markdown-overlays buffer)
  (setf (lem:buffer-value buffer 'markdown-rows) nil)
  (let* ((ranges (shown-lines buffer))
         (counts (make-list (length ranges) :initial-element 0))
         (window (first (buffer-windows buffer)))
         (cursor (cursor-line buffer))
         (folded (folded-lines buffer))
         (width (* *char-width* (max 10 (- (if window (lem:window-width window) 80) 2))))
         (fence nil))
    (lem:with-point ((point (lem:buffer-start-point buffer)))
      (loop for line from 1
            do (let* ((string (lem:line-string point))
                      (hidden (and (< 1 line) (hidden-line-p point)))
                      ;; shown by a window: from its first line, as many as it shows
                      (shown (and (not hidden)
                                  (loop for (from . lines) in ranges
                                        for cell on counts
                                        thereis (and (<= from line) (< (car cell) lines)))))
                      (decorator (make-decorator :point point :line line
                                                 :reveal (eql line cursor) :width width
                                                 :folded (member line folded)))
                      (this-fence (fence-p string)))
                 (unless hidden
                   (loop for (from) in ranges
                         for cell on counts
                         when (<= from line) do (incf (car cell))))
                 (cond
                   ;; ``` lang: the language small, the marks hidden
                   ((and this-fence (or (null fence) (string= fence this-fence)))
                    (when shown
                      (add-line-background decorator :code-block)
                      (let ((marks (+ (search this-fence string) 3)))
                        (if (decorator-folded decorator)
                            (show-as decorator 0 marks (list "▸ " (md-attribute :bullet)))
                            (hide decorator 0 marks))
                        (add-overlay decorator marks (length string) :fence)))
                    (setf fence (if fence nil this-fence)))
                   (fence
                    (when shown (add-line-background decorator :code-block)))
                   (shown
                    (decorate-line decorator string))))
               ;; (past what every window shows)
               (unless (and (lem:line-offset point 1)
                            (loop for (nil . lines) in ranges
                                  for count in counts
                                  thereis (< count lines)))
                 (return))))))

;;; ---- folding

(defun heading-level (string)
  "The level of the heading STRING (1 for #), or NIL."
  (when (ppcre:scan "^#{1,6}[ \t]" string)
    (position #\# string :test-not #'char=)))

(defun section-end (point level)
  "The point where the section of the heading of LEVEL at POINT ends: the
start of the next heading of LEVEL or higher (headings in code blocks are
not headings), or the end of the buffer.  With LEVEL 6, the next heading."
  (let ((end (lem:copy-point point :temporary))
        (fence nil))
    (loop
      (unless (lem:line-offset end 1)
        (lem:buffer-end end)
        (return end))
      (let* ((string (lem:line-string end))
             (this-fence (fence-p string)))
        (cond ((and this-fence (or (null fence) (string= fence this-fence)))
               (setf fence (if fence nil this-fence)))
              ((and (null fence)
                    (let ((other (heading-level string)))
                      (and other (<= other level))))
               (return end)))))))

(defun fence-end (point fence)
  "The start of the line after the code block that the fence at POINT opens."
  (let ((end (lem:copy-point point :temporary)))
    (loop
      (unless (lem:line-offset end 1)
        (lem:buffer-end end)
        (return end))
      (when (equal (fence-p (lem:line-string end)) fence)
        (unless (lem:line-offset end 1) (lem:line-end end))
        (return end)))))

(defun in-code-block-p (point)
  "Whether the line of POINT is in a code block (after an odd number of fences)."
  (let ((fence nil))
    (lem:with-point ((p (lem:buffer-start-point (lem:point-buffer point))))
      (loop until (lem:same-line-p p point)
            do (let ((this (fence-p (lem:line-string p))))
                 (when (and this (or (null fence) (string= fence this)))
                   (setf fence (if fence nil this))))
               (unless (lem:line-offset p 1) (return))))
    fence))

(defun markdown-fold-region (point &key (level nil))
  "The region that folds at the line of POINT, (values start end), or NIL:
the section of a heading (with LEVEL, up to the next heading of LEVEL or
higher), or a code block from its opening fence."
  (let* ((start (lem:copy-point point :temporary))
         (string (lem:line-string start)))
    (lem:line-start start)
    (let ((end (unless (in-code-block-p start)
                 (let ((heading (heading-level string))
                       (fence (fence-p string)))
                   (cond (heading (section-end start (or level heading)))
                         (fence (fence-end start fence)))))))
      ;; (not the blank lines before the next heading: they stay)
      (when end
        (lem:with-point ((last end))
          (loop while (and (lem:line-offset last -1)
                           (lem:point< start last)
                           (string= "" (string-trim " " (lem:line-string last))))
                do (lem:move-point end last))))
      (when (and end (not (lem:same-line-p start end)))
        (values start end)))))

(defun fold-at (point &key level)
  "Fold the region at the line of POINT; the fold, or NIL."
  (multiple-value-bind (start end) (markdown-fold-region point :level level)
    (when start
      (lem:place-region-placeholder-overlay start end :placeholder " ⋯"))))

(defun fold-header-p (point)
  (some (lambda (overlay)
          (and (lem:overlay-get overlay :fold)
               (lem:same-line-p (lem:overlay-start overlay) point)))
        (lem:buffer-overlays (lem:point-buffer point))))

(defun unfold-buffer (buffer)
  (dolist (overlay (copy-list (lem:buffer-overlays buffer)))
    (when (lem:overlay-get overlay :fold)
      (lem:delete-overlay overlay))))

(lem:define-command markdown-fold-or-indent () ()
  "On a heading or a code block, fold or unfold it; elsewhere, indent."
  (let ((point (lem:current-point)))
    (cond ((fold-header-p point)
           (lem/language-mode:fold-toggle-at-point point))
          ((markdown-fold-region point)
           (fold-at point)
           (lem:line-start point))
          (t (lem:call-command "indent-line-and-complete-symbol" nil)))))

(defun fold-headings (buffer function)
  "Fold at each heading of BUFFER (not in code blocks) for which FUNCTION of
its level returns true, or a level to fold its section to."
  (let ((fence nil))
    (lem:with-point ((point (lem:buffer-start-point buffer)))
      (loop
        (let* ((string (lem:line-string point))
               (this-fence (fence-p string))
               (level (and (null fence) (heading-level string))))
          (cond ((and this-fence (or (null fence) (string= fence this-fence)))
                 (setf fence (if fence nil this-fence)))
                ((and level (not (hidden-line-p point)))
                 (let ((fold (funcall function level)))
                   (when fold
                     (fold-at point :level (if (integerp fold) fold level)))))))
        (unless (lem:line-offset point 1) (return))))))

(lem:define-command markdown-cycle-folds () ()
  "Show the outline of the buffer (its first headings), then all its
headings, then all of it."
  (let* ((buffer (lem:current-buffer))
         (state (lem:buffer-value buffer 'markdown-fold-cycle))
         (top (let ((levels '()))
                (fold-headings buffer (lambda (level) (push level levels) nil))
                (and levels (reduce #'min levels)))))
    (unfold-buffer buffer)
    (setf (lem:buffer-value buffer 'markdown-fold-cycle)
          (case state
            ((nil :all)
             ;; the outline: the sections of the first headings folded
             (fold-headings buffer (lambda (level) (= level top)))
             (lem:message "Outline")
             :outline)
            (:outline
             ;; the headings: what is under each, up to the next one
             (fold-headings buffer (lambda (level) (declare (ignore level)) 6))
             (lem:message "Headings")
             :headings)
            (t
             (lem:message "All")
             :all)))
    (let ((point (lem:current-point)))
      (when (hidden-line-p point)
        (loop while (and (hidden-line-p point) (lem:line-offset point -1))))
      (lem:line-start point))))

;;; ---- the mode

(lem:define-minor-mode markdown-live-mode
    (:name "Live"
     :description "Show Markdown as it reads, its marks on the line of the cursor."
     :keymap *markdown-live-mode-keymap*
     :enable-hook 'markdown-live-on
     :disable-hook 'markdown-live-off))

(defvar *markdown-live-by-default* t
  "Whether Markdown buffers are shown with markdown-live-mode.")

(defun markdown-live-on ()
  (setf (lem:buffer-value (lem:current-buffer) 'markdown-state) nil
        (lem:variable-value 'lem/language-mode:fold-region-function :buffer (lem:current-buffer))
        'markdown-fold-region)
  (refresh-markdown-buffers :force t))

(defun markdown-live-off ()
  (let ((buffer (lem:current-buffer)))
    (clear-markdown-overlays buffer)
    (setf (lem:buffer-value buffer 'markdown-state) nil)))

(defun markdown-state (buffer)
  "What the overlays of BUFFER depend on: its text, the line of the cursor,
the lines and the width its windows show, the theme."
  (list (lem:buffer-modified-tick buffer)
        (cursor-line buffer)
        (shown-lines buffer)
        (folded-lines buffer)
        (mapcar #'lem:window-width (buffer-windows buffer))
        *char-width*
        (theme-key)))

;;; Lem keeps the cursor in a window counting the lines of the buffer, as
;;; if each took one line of the window: here headings and images take
;;; more, so the window is scrolled further when the cursor would be under
;;; its bottom.

(defun line-rows (buffer point columns)
  "The lines of the window that the line of POINT takes (about, when it is
not decorated: as many as it wraps to)."
  (when (and (not (lem:first-line-p point)) (hidden-line-p point))
    (return-from line-rows 0))
  (let ((noted (let ((table (lem:buffer-value buffer 'markdown-rows)))
                 (and table (gethash (lem:line-number-at-point point) table))))
        (wrapped (max 1 (ceiling (lem:string-width (lem:line-string point)) (max 1 (1- columns))))))
    (max wrapped (or noted 1))))

(defun keep-cursor-shown (window)
  "Move the view of WINDOW down until the line of its cursor is in it; true
if it moved."
  (let* ((buffer (lem:window-buffer window))
         (view (lem:window-view-point window))
         (cursor (lem:buffer-point buffer))
         (height (max 1 (1- (lem:window-height window))))
         (columns (lem:window-width window))
         (moved nil))
    (when (lem:point<= view cursor)
      (loop
        (let ((rows (lem:with-point ((p view))
                      (lem:line-start p)
                      (loop with total = 0
                            do (incf total (line-rows buffer p columns))
                            until (or (lem:same-line-p p cursor) (> total height)
                                      (not (lem:line-offset p 1)))
                            finally (return total)))))
          (if (and (> rows height) (not (lem:same-line-p view cursor)))
              (progn
                (lem:line-offset view 1)
                ;; (not into a fold)
                (loop while (and (hidden-line-p view) (lem:line-offset view 1)))
                (setf moved t))
              (return)))))
    moved))

(defun refresh-markdown-buffers (&key force)
  "Decorate again the shown buffers in markdown-live-mode whose state changed
(all of them with FORCE), and keep the cursor of the current window in it."
  (dolist (buffer (remove-duplicates (mapcar #'lem:window-buffer (lem:window-list))))
    (when (lem:mode-active-p buffer 'markdown-live-mode)
      (let ((state (markdown-state buffer)))
        (when (or force (not (equal state (lem:buffer-value buffer 'markdown-state))))
          (setf (lem:buffer-value buffer 'markdown-state) state)
          (decorate-buffer buffer)
          (let ((window (lem:current-window)))
            (when (and (eq (lem:window-buffer window) buffer)
                       (keep-cursor-shown window))
              ;; the lines now shown are decorated, and counted again
              (decorate-buffer buffer)
              (keep-cursor-shown window)
              (setf (lem:buffer-value buffer 'markdown-state) (markdown-state buffer)))))))))

(defun refresh-markdown-after-command ()
  (refresh-markdown-buffers))

(defun refresh-markdown-after-scroll (window)
  (declare (ignore window))
  (refresh-markdown-buffers))

(lem:add-hook lem:*post-command-hook* 'refresh-markdown-after-command)
(lem:add-hook lem:*window-scroll-functions* 'refresh-markdown-after-scroll)

;;; Markdown buffers turn it on: Lem's Markdown mode is loaded when it is
;;; first used (extensions.lisp), then its hook is given this.

(defun turn-on-markdown-live ()
  (when *markdown-live-by-default*
    (markdown-live-mode t)))

(defun hook-markdown-mode ()
  (let ((hook (find-symbol "*MARKDOWN-MODE-HOOK*" "LEM-MARKDOWN-MODE")))
    (when hook
      (lem:add-hook (symbol-value hook) 'turn-on-markdown-live)))
  ;; Lem's Markdown mode runs the commands in a code block in the mode of its
  ;; language: in a Lisp block, the Lisp mode, which talks to micros with the
  ;; sockets that lem-clysm-lisp brings (not the TCP of lem-lisp-mode alone)
  (load-extension "lem-clysm-lisp" :quiet t))

(push (cons "lem-markdown-mode" 'hook-markdown-mode) *after-extension-load*)

;;; ---- editing

(lem:define-key *markdown-live-mode-keymap* "C-c C-t" 'markdown-toggle-task)
(lem:define-key *markdown-live-mode-keymap* "Tab" 'markdown-fold-or-indent)
(lem:define-key *markdown-live-mode-keymap* "Shift-Tab" 'markdown-cycle-folds)

(lem:define-command markdown-toggle-task () ()
  "Check or uncheck the task (- [ ] ...) of the line."
  (let ((point (lem:current-point)))
    (lem:with-point ((p point))
      (lem:line-start p)
      (multiple-value-bind (start end rs re)
          (ppcre:scan "^\\s*(?:[-*+]|\\d+[.)])\\s+\\[([ xX])\\]" (lem:line-string p))
        (declare (ignore start end))
        (declare (ignore re))
        (unless rs (lem:editor-error "Not a task"))
        (let ((checked (char/= #\Space (char (lem:line-string p) (aref rs 0)))))
          (lem:character-offset p (aref rs 0))
          (lem:delete-character p 1)
          (lem:insert-character p (if checked #\Space #\x)))))))
