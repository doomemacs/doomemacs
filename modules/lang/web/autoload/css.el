;;; lang/web/autoload/css.el -*- lexical-binding: t; -*-

;;;###autoload
;; TODO (defun +css/scss-build ())

;;;###autoload
;; TODO (defun +css/sass-build ())

(defun +css--toggle-inline-or-block (beg end)
  (skip-chars-forward " \t")
  (let ((orig (point-marker)))
    (with-no-warnings
      (quiet!
       (if (= (line-number-at-pos beg) (line-number-at-pos end))
           (progn
             (goto-char (1+ beg)) (insert "\n")
             (replace-regexp ";\\s-+" ";\n" nil beg end)
             (indent-region beg end))
         (replace-regexp "\n" " " nil beg end)
         (replace-regexp " +" " " nil beg end))))
    (if orig (goto-char orig))))

;;;###autoload
(defun +css/toggle-inline-or-block ()
  "Toggles between a bracketed block and inline block."
  (interactive)
  (let ((inhibit-modification-hooks t))
    (cl-destructuring-bind (&key beg end op cl &allow-other-keys)
        (save-excursion
          (when (and (eq (char-after) ?{)
                     (not (eq (char-before) ?{)))
            (forward-char))
          (sp-get-sexp))
      (when (or (not (and beg end op cl))
                (string-empty-p op) (string-empty-p cl))
        (user-error "No block found %s" (list beg end op cl)))
      (unless (string= op "{")
        (user-error "Incorrect block found"))
      (if (featurep 'evil)
          (evil-with-single-undo (+css--toggle-inline-or-block beg end))
        (+css--toggle-inline-or-block beg end)))))

;;;###autoload
(defun +css/comment-indent-new-line ()
  "Continues the comment in an indented new line in css-mode and scss-mode.
Meant for `comment-line-break-function'."
  (interactive)
  (when (sp-point-in-comment)
    (let ((at-end (looking-at-p ".+\\*/"))
          type pre-indent post-indent)
      (save-excursion
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (if (not comment-use-syntax)
              (progn
                (goto-char bol)
                (when (re-search-forward comment-start-skip eol t)
                  (goto-char (or (match-end 1) (match-beginning 0)))))
            (goto-char (comment-beginning))))
        (save-match-data
          (looking-at "\\(//\\|/?\\*\\)")
          (setq type (match-string 0)
                pre-indent (- (match-beginning 0) (line-beginning-position))
                post-indent
                (progn
                  (goto-char (match-end 0))
                  (max 1 (skip-chars-forward " " (line-end-position)))))
          (if (eolp) (setq post-indent 1))))
      (insert "\n"
              (make-string pre-indent 32)
              (if (string= "/*" type)
                  " *"
                type)
              (make-string post-indent 32))
      (when at-end
        (save-excursion
          (insert "\n" (make-string pre-indent 32))
          (delete-char -1))))))
