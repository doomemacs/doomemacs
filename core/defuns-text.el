;;;###autoload
(defun my--point-at-bol-non-blank()
  (save-excursion (evil-first-non-blank) (point)))

;;;###autoload
(defun my--surrounded-p ()
  (and (looking-back "[[{(]\\(\s+\\|\n\\)?\\(\s\\|\t\\)*")
       (let* ((whitespace (match-string 1))
              (match-str (concat whitespace (match-string 2) "[])}]")))
         (looking-at-p match-str))))

;;;###autoload
(defun my.backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line (sp-point-in-blank-line)))
    (evil-delete (point-at-bol) (point))
    (if (not empty-line)
        (indent-according-to-mode))))

;;;###autoload
(defun my.move-to-bol ()
  "Moves cursor to the first non-blank character on the line. If
already there, move it to the true bol."
  (interactive)
  (evil-save-goal-column
    (let ((point-at-bol (my--point-at-bol-non-blank))
          (point (point)))
      (if (= point-at-bol point)
          (evil-move-beginning-of-line)
        (unless (= (point-at-bol) point)
          (evil-first-non-blank))))))

;;;###autoload
(defun my.move-to-eol ()
  (interactive)
  (evil-save-goal-column
    (let ((old-point (point)))
      (when (comment-search-forward (point-at-eol) t)
        (goto-char (match-beginning 0))
        (skip-syntax-backward " ^<*" (my--point-at-bol-non-blank))

        (if (eq old-point (point)) ;
            (evil-move-end-of-line))))))

;; Mimic expandtab in vim
;;;###autoload
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
               (if (string-match "\\w*\\(\\s-+\\)$"
                                 (buffer-substring-no-properties (- p movement) p))
                   (backward-delete-char (- (match-end 1) (match-beginning 1)))
                 (call-interactively 'backward-delete-char-untabify)))))))

;;;###autoload
(defun my.dumb-indent ()
  "Inserts a tab character (or spaces x tab-width). Checks if the
auto-complete window is open."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (zerop movement) tab-width (- tab-width movement))))
      (insert (s-repeat spaces " ")))))

;;;###autoload
(defun my.inflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so."
  (interactive)
  (if (my--surrounded-p)
      (progn (call-interactively 'self-insert-command)
             (save-excursion (call-interactively 'self-insert-command)))
    (call-interactively 'self-insert-command)))

;;;###autoload
(defun my.deflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`my.backward-delete-whitespace-to-column' otherwise."
  (interactive)
  (save-match-data
    (if (my--surrounded-p)
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

;;;###autoload
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
