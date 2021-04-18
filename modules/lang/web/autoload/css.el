;;; lang/web/autoload/css.el -*- lexical-binding: t; -*-

;; ;;;###autoload
;; TODO (defun +css/scss-build ())

;; ;;;###autoload
;; TODO (defun +css/sass-build ())

(defun +css--toggle-inline-or-block (beg end)
  (skip-chars-forward " \t")
  (let ((orig (point-marker)))
    (goto-char beg)
    (if (= (line-number-at-pos beg) (line-number-at-pos end))
        (progn
          (forward-char)
          (insert "\n")
          (while (re-search-forward ";\\s-+" end t)
            (replace-match ";\n" nil t))
          (indent-region beg end))
      (save-excursion
        (while (re-search-forward "\n+" end t)
          (replace-match " " nil t)))
      (while (re-search-forward "\\([{;]\\) +" end t)
        (replace-match (concat (match-string 1) " ") nil t)))
    (if orig (goto-char orig))
    (skip-chars-forward " \t")))

;;;###autoload
(defun +css/toggle-inline-or-block ()
  "Toggles between a bracketed block and inline block."
  (interactive)
  (let ((inhibit-modification-hooks t))
    (cl-destructuring-bind (&key beg end op cl &allow-other-keys)
        (save-excursion
          (when (and (eq (char-after) ?\{)
                     (not (eq (char-before) ?\{)))
            (forward-char))
          (sp-get-sexp))
      (when (or (not (and beg end op cl))
                (string-empty-p op) (string-empty-p cl))
        (user-error "No block found %s" (list beg end op cl)))
      (unless (string= op "{")
        (user-error "Incorrect block found"))
      (+css--toggle-inline-or-block beg end))))

;;;###autoload
(defun +css/comment-indent-new-line (&optional _)
  "Continues the comment in an indented new line.

Meant for `comment-line-break-function' in `css-mode' and `scss-mode'."
  (interactive)
  (cond ((or (not (doom-point-in-comment-p))
             (and comment-use-syntax
                  (not (save-excursion (comment-beginning)))))
         (let (comment-line-break-function)
           (newline-and-indent)))

        ((save-match-data
           (let ((at-end (looking-at-p ".+\\*/"))
                 (indent-char (if indent-tabs-mode ?\t ?\s))
                 (post-indent (save-excursion
                                (move-to-column (1+ (current-indentation)))
                                (skip-chars-forward " \t" (line-end-position))))
                 (pre-indent (current-indentation))
                 opener)
             (save-excursion
               (if comment-use-syntax
                   (goto-char (comment-beginning))
                 (goto-char (line-beginning-position))
                 (when (re-search-forward comment-start-skip (line-end-position) t)
                   (goto-char (or (match-end 1)
                                  (match-beginning 0)))))
               (buffer-substring-no-properties (point) (line-end-position))
               (when (looking-at "\\(//\\|/?\\*\\**/?\\)\\(?:[^/]\\)")
                 (list (match-string-no-properties 1)
                       (- (match-beginning 1) (line-beginning-position))))
               (if (looking-at "\\(//\\|/?\\*\\**/?\\)\\(?:[^/]\\)")
                   (setq opener (match-string-no-properties 1)
                         pre-indent (- (match-beginning 1) (line-beginning-position)))
                 (setq opener ""
                       pre-indent 0)))
             (insert-and-inherit
              "\n" (make-string pre-indent indent-char)
              (if (string-prefix-p "/*" opener)
                  (if (or (eq +web-continue-block-comments t)
                          (string= "/**" opener))
                      " *"
                    "")
                opener)
              (make-string post-indent indent-char))
             (when at-end
               (save-excursion
                 (just-one-space)
                 (insert "\n" (make-string pre-indent indent-char)))))))))

;;;###autoload
(defun +css-adaptive-fill-fn ()
  "An `adaptive-fill-function' that conjoins SCSS line comments correctly."
  (when (looking-at "[ \t]*/[/*][ \t]*")
    (let ((str (match-string 0)))
      (when (string-match "/[/*]" str)
        (replace-match (if (string= (match-string 0 str) "/*")
                           " *"
                         "//")
                       t t str)))))
