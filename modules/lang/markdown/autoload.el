;;; lang/markdown/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +markdown-flyspell-word-p ()
  "Return t if point is on a word that should be spell checked.

Return nil if on a link url, markup, html, or references."
  (let ((faces (doom-enlist (get-text-property (point) 'face))))
    (or (and (memq 'font-lock-comment-face faces)
             (memq 'markdown-code-face faces))
        (not (cl-loop with unsafe-faces = '(markdown-reference-face
                                            markdown-url-face
                                            markdown-markup-face
                                            markdown-comment-face
                                            markdown-html-attr-name-face
                                            markdown-html-attr-value-face
                                            markdown-html-tag-name-face
                                            markdown-code-face)
                      for face in faces
                      if (memq face unsafe-faces)
                      return t)))))


;;
;;; Compilation handlers

;;;###autoload
(defun +markdown-compile (beg end output-buffer)
  "Compile markdown into html.

Runs `+markdown-compile-functions' until the first function to return non-nil,
otherwise throws an error."
  (or (run-hook-with-args-until-success '+markdown-compile-functions
                                        beg end output-buffer)
      (user-error "No markdown program could be found. Install marked, pandoc, markdown or multimarkdown.")))

;;;###autoload
(defun +markdown-compile-marked (beg end output-buffer)
  "Compiles markdown with the marked program, if available.
Returns its exit code."
  (when (executable-find "marked")
    (apply #'call-process-region
           beg end "marked" nil output-buffer nil
           (when (eq major-mode 'gfm-mode)
             (list "--gfm" "--tables" "--breaks")))))

;;;###autoload
(defun +markdown-compile-pandoc (beg end output-buffer)
  "Compiles markdown with the pandoc program, if available.
Returns its exit code."
  (when (executable-find "pandoc")
    (call-process-region beg end "pandoc" nil output-buffer nil
                         "-f" "markdown"
                         "-t" "html"
                         "--mathjax"
                         "--highlight-style=pygments")))

;;;###autoload
(defun +markdown-compile-multimarkdown (beg end output-buffer)
  "Compiles markdown with the multimarkdown program, if available. Returns its
exit code."
  (when (executable-find "multimarkdown")
    (call-process-region beg end "multimarkdown" nil output-buffer)))

;;;###autoload
(defun +markdown-compile-markdown (beg end output-buffer)
  "Compiles markdown using the Markdown.pl script (or markdown executable), if
available. Returns its exit code."
  (when-let (exe (or (executable-find "Markdown.pl")
                     (executable-find "markdown")))
    (call-process-region beg end exe nil output-buffer nil)))


;;
;;; Commands

;;;###autoload
(defun +markdown/insert-del ()
  "Surround region in github strike-through delimiters."
  (interactive)
  (let ((regexp "\\(^\\|[^\\]\\)\\(\\(~\\{2\\}\\)\\([^ \n	\\]\\|[^ \n	]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)")
        (delim "~~"))
    (if (markdown-use-region-p)
        ;; Active region
        (cl-destructuring-bind (beg . end)
            (markdown-unwrap-things-in-region
             (region-beginning) (region-end)
             regexp 2 4)
          (markdown-wrap-or-insert delim delim nil beg end))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at regexp)
          (markdown-unwrap-thing-at-point nil 2 4)
        (markdown-wrap-or-insert delim delim 'word nil nil)))))
