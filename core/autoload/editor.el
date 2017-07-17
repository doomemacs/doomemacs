;;; core/autoload/editor.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/sudo-find-file (file)
  "Open a file as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo:root@localhost:" file))))

;;;###autoload
(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (doom/sudo-find-file (file-truename buffer-file-name)))

(defun doom--goto-first-non-blank ()
  (beginning-of-visual-line)
  (skip-chars-forward " \t\r"))

;;;###autoload
(defun doom/backward-to-bol-or-indent ()
  "Move back to the current line's indentation. If already there, move to the
beginning of the line instead. If at bol, do nothing."
  (interactive)
  (let ((boi (save-excursion (back-to-indentation) (point)))
        (point (point)))
    (if (= boi point)
        (beginning-of-visual-line)
      (unless (= (line-beginning-position) point)
        (doom--goto-first-non-blank)))))

;;;###autoload
(defun doom/forward-to-last-non-comment-or-eol ()
  "Move forward to the last non-blank character in the line, ignoring comments
and trailing whitespace. If already there, move to the real end of the line.
If already there, do nothing."
  (interactive)
  (let* ((point (point))
         (eol (save-excursion (end-of-visual-line) (point)))
         (bol (save-excursion (beginning-of-visual-line) (point)))
         (eoc (or (if (not comment-use-syntax)
                      (when (re-search-forward comment-start-skip eol t)
                        (or (match-end 1) (match-beginning 0)))
                    (save-excursion
                      (goto-char eol)
                      (while (and (sp-point-in-comment)
                                  (> (point) point))
                        (backward-char))
                      (when (> (point) point)
                        (skip-chars-backward " " bol)
                        (point))))
                  eol))
         (goto-char-fn (if (featurep 'evil) #'evil-goto-char #'goto-char)))
    (if (= eoc point)
        (funcall goto-char-fn eol)
      (unless (= eol point)
        (funcall goto-char-fn eoc)))))

(defun doom--surrounded-p ()
  (and (looking-back "[[{(]\\(\s+\\|\n\\)?\\(\s\\|\t\\)*" (line-beginning-position))
       (let* ((whitespace (match-string 1))
              (match-str (concat whitespace (match-string 2) "[])}]")))
         (looking-at-p match-str))))

;;;###autoload
(defun doom/dumb-indent ()
  "Inserts a tab character (or spaces x tab-width)."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (= 0 movement) tab-width (- tab-width movement))))
      (insert (make-string spaces ? )))))

;;;###autoload
(defun doom/dumb-dedent ()
  "Dedents the current line."
  (interactive)
  (if indent-tabs-mode
      (call-interactively #'backward-delete-char)
    (save-excursion
      (unless (looking-back "^[\s\t]*" (line-beginning-position))
        (doom--goto-first-non-blank))
      (let* ((movement (% (current-column) tab-width))
             (spaces (if (= 0 movement) tab-width (- tab-width movement))))
        (delete-char (- spaces))))))

;;;###autoload
(defun doom/backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line (save-excursion (beginning-of-line) (looking-at-p "[ \t]*$"))))
    (funcall (if (featurep 'evil) #'evil-delete #'delete-region)
             (point-at-bol) (point))
    (unless empty-line
      (indent-according-to-mode))))

;;;###autoload
(defun doom/backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((delete-backward-char (if (derived-mode-p 'org-mode)
                                   #'org-delete-backward-char
                                 #'delete-backward-char))
         (context (sp--get-pair-list-context 'navigate))
         (open-pair-re (sp--get-opening-regexp context))
         (close-pair-re (sp--get-closing-regexp context))
         open-len close-len)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
          ;; Also, skip closing delimiters
          ((and (and (sp--looking-back open-pair-re)
                     (setq open-len (- (match-beginning 0) (match-end 0))))
                (and (looking-at close-pair-re)
                     (setq close-len (- (match-beginning 0) (match-end 0))))
                (string= (plist-get (sp-get-thing t) :op)
                         (plist-get (sp-get-thing) :cl)))
           (delete-char (- 0 open-len))
           (delete-char close-len))

          ;; Delete up to the nearest tab column IF only whitespace between
          ;; point and bol.
          ((save-match-data (looking-back "^[\\t ]*" (line-beginning-position)))
           (let ((movement (% (current-column) tab-width))
                 (p (point)))
             (when (= movement 0)
               (setq movement tab-width))
             (save-match-data
               (if (string-match "\\w*\\(\\s-+\\)$"
                                 (buffer-substring-no-properties (max (point-min) (- p movement)) p))
                   (sp-delete-char (- 0 (- (match-end 1) (match-beginning 1))))
                 (call-interactively delete-backward-char)))))

          ;; Otherwise do a regular delete
          (t (call-interactively delete-backward-char)))))

;;;###autoload
(defun doom/inflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so."
  (interactive)
  (let ((command (or (command-remapping #'self-insert-command)
                     #'self-insert-command)))
    (cond ((doom--surrounded-p)
           (call-interactively command)
           (save-excursion (call-interactively command)))
          (t
           (call-interactively command)))))

;;;###autoload
(defun doom/deflate-space-maybe ()
  "Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`doom/backward-delete-whitespace-to-column' otherwise."
  (interactive)
  (save-match-data
    (cond ((doom--surrounded-p)
           (let ((whitespace-match (match-string 1)))
             (cond ((not whitespace-match)
                    (call-interactively #'delete-backward-char))
                   ((string-match "\n" whitespace-match)
                    (funcall (if (featurep 'evil) #'evil-delete #'delete-region)
                             (point-at-bol) (point))
                    (call-interactively #'delete-backward-char)
                    (save-excursion (call-interactively #'delete-char)))
                   (t (just-one-space 0)))))
          (t
           (doom/backward-delete-whitespace-to-column)))))

;;;###autoload
(defun doom/newline-and-indent ()
  "Inserts a newline and possibly indents it. Also continues comments if
executed from a commented line; handling special cases for certain languages
with weak native support."
  (interactive)
  (cond ((sp-point-in-string)
         (newline))
        ((sp-point-in-comment)
         (cond ((memq major-mode '(js2-mode rjsx-mode))
                (call-interactively #'js2-line-break))
               ((memq major-mode '(java-mode php-mode))
                (c-indent-new-comment-line))
               ((memq major-mode '(c-mode c++-mode objc-mode css-mode scss-mode js2-mode))
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

;;;###autoload
(defun doom/retab (&optional beg end)
  "Changes all tabs to spaces or spaces to tabs, so that indentation is
consistent throughout a selected region, depending on `indent-tab-mode'."
  (interactive "r")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))

;;;###autoload
(defun doom/toggle-sticky (&optional beg end)
  "Make a selection sticky by placing it in the header line. Possibly helpful
for function signatures or notes. Run again to clear the header line."
  (interactive "r")
  (setq header-line-format
        (when mark-active
          (concat (propertize (format linum-format (line-number-at-pos beg))
                              'face 'font-lock-comment-face)
                  (let ((content (buffer-substring beg end)))
                    (setq content (replace-regexp-in-string "\n" " " content t t))
                    (setq content (replace-regexp-in-string "\\s-+" " " content))
                    content)))))

;;;###autoload
(defun doom/scratch-buffer (&optional arg)
  "Opens the scratch buffer in a popup window.

If ARG (universal argument) is non-nil, open it in the current window instead of
a popup.

If a region is active, copy it into the scratch buffer."
  (interactive "P")
  (let ((text (and (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))))
        (mode major-mode)
        (derived-p (derived-mode-p 'prog-mode 'text-mode))
        (old-project (doom-project-root))
        (new-buf (get-buffer-create "*doom:scratch*")))
    (if arg
        (switch-to-buffer new-buf)
      (doom-popup-buffer new-buf))
    (with-current-buffer new-buf
      (setq default-directory old-project)
      (when (and (not (eq major-mode mode))
                 derived-p
                 (functionp mode))
        (funcall mode))
      (if text (insert text)))))

;;;###autoload
(defun doom|enable-delete-trailing-whitespace ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
