;;; core/autoload/editor.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/sudo-find-file (file)
  "Open FILE as root."
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

;;;###autoload
(defun doom/backward-to-bol-or-indent ()
  "Jump between the indentation column (first non-whitespace character) and the
beginning of the line. The opposite of
`doom/forward-to-last-non-comment-or-eol'."
  (interactive)
  (let ((pos (point))
        (indent (save-excursion
                  (beginning-of-visual-line)
                  (skip-chars-forward " \t\r")
                  (point))))
    (cond ((or (> pos indent) (= pos (line-beginning-position)))
           (goto-char indent))
          ((<= pos indent)
           (beginning-of-visual-line)))))

;;;###autoload
(defun doom/forward-to-last-non-comment-or-eol ()
  "Jumps between the last non-blank, non-comment character in the line and the
true end of the line. The opposite of `doom/backward-to-bol-or-indent'."
  (interactive)
  (let ((eol (save-excursion (end-of-visual-line) (point))))
    (if (and (sp-point-in-comment) (not (= (point) eol)))
        (goto-char eol)
      (let* ((bol (save-excursion (beginning-of-visual-line) (point)))
             (boc (or (save-excursion
                        (if (not comment-use-syntax)
                            (progn
                              (goto-char bol)
                              (when (re-search-forward comment-start-skip eol t)
                                (or (match-end 1) (match-beginning 0))))
                          (goto-char eol)
                          (while (and (sp-point-in-comment)
                                      (> (point) bol))
                            (backward-char))
                          (skip-chars-backward " " bol)
                          (point)))
                      eol)))
        (cond ((= boc (point))
               (goto-char eol))
              ((/= bol boc)
               (goto-char boc)))))))

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
    (unless (bolp)
      (save-excursion
        (when (> (current-column) (current-indentation))
          (back-to-indentation))
        (let ((movement (% (current-column) tab-width)))
          (delete-char
           (- (if (= 0 movement)
                  tab-width
                (- tab-width movement)))))))))

;;;###autoload
(defun doom/backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1."
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (funcall (if (featurep 'evil)
                 #'evil-delete
               #'delete-region)
             (point-at-bol) (point))
    (unless empty-line-p
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
                   (sp-delete-char
                    (- 0 (- (match-end 1)
                            (match-beginning 1))))
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
    (if (doom--surrounded-p)
        (let ((whitespace-match (match-string 1)))
          (cond ((not whitespace-match)
                 (call-interactively #'delete-backward-char))
                ((string-match "\n" whitespace-match)
                 (funcall (if (featurep 'evil)
                              #'evil-delete
                            #'delete-region)
                          (point-at-bol) (point))
                 (call-interactively #'delete-backward-char)
                 (save-excursion (call-interactively #'delete-char)))
                (t (just-one-space 0))))
      (doom/backward-delete-whitespace-to-column))))

;;;###autoload
(defun doom/newline-and-indent ()
  "Inserts a newline and possibly indents it. Also continues comments if
executed from a commented line; handling special cases for certain languages
with weak native support."
  (interactive)
  (cond ((sp-point-in-string)
         (newline))
        ((sp-point-in-comment)
         (pcase major-mode
           ((or 'js2-mode 'rjsx-mode)
            (call-interactively #'js2-line-break))
           ((or 'java-mode 'php-mode)
            (c-indent-new-comment-line))
           ((or 'c-mode 'c++-mode 'objc-mode 'css-mode 'scss-mode 'js2-mode)
            (newline-and-indent)
            (insert "* ")
            (indent-according-to-mode))
           (_
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

(defvar-local doom--buffer-narrowed-origin nil)
;;;###autoload
(defun doom/narrow-buffer (beg end &optional clone-p)
  "Restrict editing in this buffer to the current region, indirectly. With CLONE-P,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "r")
  (cond ((region-active-p)
         (deactivate-mark)
         (when clone-p
           (let ((old-buf (current-buffer)))
             (switch-to-buffer (clone-indirect-buffer nil nil))
             (setq doom--buffer-narrowed-origin old-buf)))
         (narrow-to-region beg end))
        (doom--buffer-narrowed-origin
         (kill-this-buffer)
         (switch-to-buffer doom--buffer-narrowed-origin)
         (setq doom--buffer-narrowed-origin nil))
        (t
         (widen))))

;;;###autoload
(defun doom|enable-delete-trailing-whitespace ()
  "Attaches `delete-trailing-whitespace' to a buffer-local `before-save-hook'."
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
