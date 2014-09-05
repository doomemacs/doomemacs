;;; Library Defuns ;;;;;;;;;;;;;;;;;;;;;
(defun my/surrounded-p ()
  (and (looking-back "[[{(]\s*")
       (looking-at-p "\s*[]})]"))
  ;; (and (s-match "[[{(]" (s-right 1 (s-trim (buffer-substring (line-beginning-position) (point)))))
  ;;      (s-match "[])}]" (s-left 1 (s-trim (buffer-substring (point) (line-end-position))))))
  )

(defun my/empty-line-p ()
  (zerop (length (s-trim (my/get-line)))))

(defun my/get-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))

;;; Text Defuns ;;;;;;;;;;;;;;;;;;;;;;;;
(defun my.backward-kill-to-bol ()
  (interactive)
  (evil-delete (point-at-bol) (point)))

(defun my.backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line (my/empty-line-p)))
    (my.backward-kill-to-bol)
    (if (not empty-line)
        (indent-according-to-mode))))

;; Mimic expandtab in vim
(defun my.backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much
whitespace as possible, or just one char (using `autopair-backspace')
if that's not possible."
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
        (call-interactively 'autopair-backspace))))))

;; TODO Make inflate/deflate smarter
(defun my.inflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so."
  (interactive)
  (if (my/surrounded-p)
      (progn (insert " ") (save-excursion (insert " ")))
    (insert " ")))

(defun my.deflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`my.backward-delete-whitespace-to-column' otherwise."
  (interactive)
  (if (my/surrounded-p)
      (progn (delete-char -1) (save-excursion (delete-char 1)))
    (my.backward-delete-whitespace-to-column)))

(defun my.dumb-indent ()
  "Inserts a tab character (or spaces x tab-width). Checks if the
auto-complete window is open."
  (interactive)
  (when (not (ac-menu-live-p))
    (let ((indent-mode indent-tabs-mode))
      (insert (if indent-mode "\t" (make-string tab-width ? ))))))

(defun my.newline-and-indent ()
  "Newline and indent; if in a comment, auto-comment and properly
indent the next line."
  (interactive)
  (let ((in-comment (evil-in-comment-p)))
    (if in-comment
        (indent-new-comment-line)
      (evil-ret-and-indent))))

(defun my.minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
