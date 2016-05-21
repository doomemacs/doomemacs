;;; defuns-whitespace.el

;;;###autoload
(defun doom--point-at-bol-non-blank()
  (save-excursion (evil-first-non-blank) (point)))

;;;###autoload
(defun doom/surrounded-p ()
  (and (looking-back "[[{(]\\(\s+\\|\n\\)?\\(\s\\|\t\\)*")
       (let* ((whitespace (match-string 1))
              (match-str (concat whitespace (match-string 2) "[])}]")))
         (looking-at-p match-str))))

;;;###autoload
(defun doom/backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line (sp-point-in-blank-line)))
    (evil-delete (point-at-bol) (point))
    (if (not empty-line)
        (indent-according-to-mode))))

;;;###autoload
(defun doom/move-to-bol ()
  "Moves cursor to the first non-blank character on the line. If
already there, move it to the true bol."
  (interactive)
  (evil-save-goal-column
    (let ((point-at-bol (doom--point-at-bol-non-blank))
          (point (point)))
      (if (= point-at-bol point)
          (evil-move-beginning-of-line)
        (unless (= (point-at-bol) point)
          (evil-first-non-blank))))))

;;;###autoload
(defun doom/move-to-eol ()
  (interactive)
  (evil-save-goal-column
    (let ((old-point (point)))
      (when (comment-search-forward (point-at-eol) t)
        (goto-char (match-beginning 0))
        (skip-syntax-backward " ^<*" (doom--point-at-bol-non-blank))

        (if (eq old-point (point)) ;
            (evil-move-end-of-line))))))

;; Mimic expandtab in vim
;;;###autoload
(defun doom/backward-delete-whitespace-to-column ()
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
        ;; Delete up to the nearest tab column IF only whitespace between point
        ;; and bol.
        ((looking-back "^[\\t ]*" (point-at-bol))
         (let ((movement (% (current-column) tab-width))
               (p (point)))
           (when (= movement 0)
             (setq movement tab-width))
           (save-match-data
             (if (string-match "\\w*\\(\\s-+\\)$"
                               (buffer-substring-no-properties (- p movement) p))
                 (backward-delete-char (- (match-end 1) (match-beginning 1)))
               (backward-delete-char-untabify 1)))))
        ;; Otherwise do a regular delete
        (t (backward-delete-char-untabify 1))))

;;;###autoload
(defun doom/dumb-indent (&optional smart)
  "Inserts a tab character (or spaces x tab-width). Checks if the
auto-complete window is open."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (zerop movement) tab-width (- tab-width movement))))
      (insert (s-repeat spaces " ")))))

;;;###autoload
(defun doom/smart-indent ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (doom/dumb-indent)))

;;;###autoload
(defun doom/dumb-dedent ()
  (interactive)
  (if indent-tabs-mode
      (delete-char -1)
    (save-excursion
      (unless (looking-back "^[\s\t]*")
        (evil-first-non-blank))
      (let* ((movement (% (current-column) tab-width))
             (spaces (if (zerop movement) tab-width (- tab-width movement))))
        (delete-char (- spaces))))))

;;;###autoload
(defun doom/inflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so."
  (interactive)
  (if (doom/surrounded-p)
      (progn (call-interactively 'self-insert-command)
             (save-excursion (call-interactively 'self-insert-command)))
    (call-interactively 'self-insert-command)))

;;;###autoload
(defun doom/deflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`doom/backward-delete-whitespace-to-column' otherwise."
  (interactive)
  (save-match-data
    (if (doom/surrounded-p)
        (let ((whitespace-match (match-string 1)))
          (cond ((not whitespace-match)
                 (call-interactively 'sp-backward-delete-char))
                ((string-match "\n" whitespace-match)
                 (evil-delete (point-at-bol) (point))
                 (delete-char -1)
                 (save-excursion (delete-char 1)))
                (t
                 (just-one-space 0))))
      (doom/backward-delete-whitespace-to-column))))

;;;###autoload
(defun doom/newline-and-indent ()
  (interactive)
  (cond ((sp-point-in-string)
         (newline))
        ((sp-point-in-comment)
         (cond ((eq major-mode 'js2-mode)
                (js2-line-break))
               ((-contains? '(java-mode php-mode) major-mode)
                (c-indent-new-comment-line))
               ((-contains? '(c-mode c++-mode objc-mode css-mode scss-mode) major-mode)
                (newline-and-indent)
                (insert "* ")
                (indent-according-to-mode))
               (t
                ;; Fix an off-by-one cursor-positioning issue
                ;; with `indent-new-comment-line'
                (let ((col (save-excursion (comment-beginning) (current-column))))
                  (indent-new-comment-line)
                  (unless (= col (current-column))
                    (insert " "))))))
        (t
         (newline nil t)
         (indent-according-to-mode))))

;;;###autoload (autoload 'doom:whitespace-retab "defuns-whitespace" nil t)
(evil-define-operator doom:whitespace-retab (beg end)
  "Akin to vim's retab, this changes all tabs-to-spaces or spaces-to-tabs,
  depending on `indent-tab-mode'. Untested."
  :motion nil
  :move-point nil
  :type line
  (interactive "<r>")
  (unless (and beg end)
    (setq beg (point-min))
    (setq end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))

;;;###autoload (autoload 'doom:whitespace-align "defuns-whitespace" nil t)
(evil-define-command doom:whitespace-align (beg end &optional regexp bang)
  :repeat nil
  (interactive "<r><a><!>")
  (when regexp
    (align-regexp beg end
                  (concat "\\(\\s-*\\)" (rxt-pcre-to-elisp regexp)) 1 1)))

(provide 'defuns-whitespace)
;;; defuns-whitespace.el ends here
