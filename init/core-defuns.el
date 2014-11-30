(provide 'core-defuns)

;;;; Convenience ;;;;;;;;;;;;;;;;;;;
(defun associate-mode (match mode)
  (add-to-list 'auto-mode-alist (cons match mode)))
(defun associate-minor-mode (match mode)
  (add-to-list 'auto-minor-mode-alist (cons match mode)))

(defmacro λ (&rest body)
  `(lambda () (interactive) ,@body))
(defmacro λ! (&rest body)
  `(lambda () ,@body))
(defmacro add-hook! (hook &rest body)
  `(add-hook ,hook (lambda() ,@body)))

;; Backwards compatibility
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(defmacro after (feature &rest forms)
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

;; vimmish keymapping shortcuts
(defalias 'defcmd 'evil-ex-define-cmd)

(defmacro ibind (key command)
  `(key-chord-define evil-insert-state-map ,key ,command))

(defun bind (state &rest keys)
  (let ((state-list state)
        (is-global (or (stringp state)
                       (vectorp state)))
        keymap)

    (if is-global
        (setq keys (-insert-at 0 state keys))
      (progn
        (if (keymapp (first keys))
            (setq keymap (pop keys)))

        (if (or (keymapp state)
                (not (listp state)))
            (setq state-list (list state)))))

    (while keys
      (let ((-key (pop keys))
            (-def (pop keys)))

        (if is-global
            (global-set-key -key -def)
          (dolist (-state state-list)
            (cond ((evil-state-p -state)
                   (define-key
                     (if keymap
                         (evil-get-auxiliary-keymap keymap -state t)
                       (evil-state-property -state :keymap t)) -key -def))
                  ((keymapp -state)
                   (define-key -state -key -def)))))))))


;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-comment-hard-wrap ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill))

(defun enable-tab-width-2 ()
  (setq tab-width 2
        evil-shift-width 2))

(defun disable-final-newline ()
  (set (make-local-variable 'require-final-newline) nil))


;;; Text Defuns ;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/surrounded-p ()
  (and (looking-back "[[{(]\\(\s+\\|\n\\)?\\(\s\\|\t\\)*")
       (let* ((whitespace (match-string 1))
              (match-str (concat whitespace (match-string 2) "[])}]")))
         (looking-at-p match-str))))

(defun my/empty-line-p ()
  (zerop (length (s-trim (my/get-line)))))

(defun my/get-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun my.backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line (my/empty-line-p)))
    (evil-delete (point-at-bol) (point))
    (if (not empty-line)
        (indent-according-to-mode))))

(defun my.point-at-first-non-blank()
  (save-excursion (evil-first-non-blank) (point)))

(defun my.move-to-bol ()
  "Moves cursor to the first non-blank character on the line. If
already there, move it to the true bol."
  (interactive)
  (evil-save-goal-column
    (let ((point-at-bol (my.point-at-first-non-blank))
          (point (point)))
      (if (= point-at-bol point)
          (evil-move-beginning-of-line)
        (unless (= (point-at-bol) point)
          (evil-first-non-blank))))))

(defun my.move-to-eol ()
  (interactive)
  (evil-save-goal-column
    (let ((old-point (point)))
      (when (comment-search-forward (point-at-eol) t)
        (goto-char (match-beginning 0))
        (skip-syntax-backward " ^<*" (my.point-at-first-non-blank))

        (if (eq old-point (point)) ;
            (evil-move-end-of-line))))))

;; Mimic expandtab in vim
(defun my.backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much
whitespace as possible, or just one char if that's not possible."
  (interactive)
  (cond ;; If in a string
        ((sp-point-in-string)
         (call-interactively 'backward-delete-char-untabify))
        ;; If using tabs (or at bol), just delete normally
        ((or indent-tabs-mode
             (= (point-at-bol) (point)))
         (call-interactively 'backward-delete-char))
        ;; Otherwise, delete up to the nearest tab column
        (t (let ((movement (% (current-column) tab-width))
                 (p (point)))
             (when (= movement 0)
               (setq movement tab-width))
             (save-match-data
               (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
                   (backward-delete-char (- (match-end 1) (match-beginning 1)))
                 (call-interactively 'backward-delete-char-untabify)))))))

(defun my.dumb-indent ()
  "Inserts a tab character (or spaces x tab-width). Checks if the
auto-complete window is open."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (zerop movement) tab-width (- tab-width movement))))
      (insert (s-repeat spaces " ")))))

(defun my.inflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so."
  (interactive)
  (if (my/surrounded-p)
      (progn (call-interactively 'self-insert-command)
             (save-excursion (call-interactively 'self-insert-command)))
    (call-interactively 'self-insert-command)))

(defun my.deflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`my.backward-delete-whitespace-to-column' otherwise."
  (interactive)
  (save-match-data
    (if (my/surrounded-p)
        (let ((whitespace-match (match-string 1)))
          (cond ((not whitespace-match)
                 (call-interactively 'delete-backward-char))
                ((string-match "\n" whitespace-match)
                 (evil-delete (point-at-bol) (point))
                 (delete-char -1)
                 (save-excursion (delete-char 1)))
                (t
                 (just-one-space 0))))
      (my.backward-delete-whitespace-to-column))))

(defun my.newline-and-indent ()
  "Newline and indent; if in a comment, auto-comment and properly
indent the next line."
  (interactive)
  (cond ((sp-point-in-string)
         (evil-ret))
        ((evil-in-comment-p)
         (if (eq major-mode 'js2-mode)
             (js2-line-break)
           (call-interactively 'indent-new-comment-line)))
        (t
         (evil-ret-and-indent))))

(defun s-lines? (s) (length (s-lines s)))


;; Utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun what-face (pos)
  "Tells you the name of the face (point) is on."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun what-col ()
  (interactive)
  (message "Column %d" (current-column)))

(defun my/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun my/project-root (&optional force-pwd)
  (if (and (not force-pwd)
           (projectile-project-p))
      (projectile-project-root)
    default-directory))

(defmacro f--exists? (file dir)
  `(f-exists? (expand-file-name ,file ,dir)))
