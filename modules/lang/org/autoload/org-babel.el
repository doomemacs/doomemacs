;;; lang/org/autoload/org-babel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-eval-handler (beg end)
  "TODO"
  (save-excursion
    (if (not (cl-loop for pos in (list beg (point) end)
                      if (save-excursion (goto-char pos) (org-in-src-block-p t))
                      return (goto-char pos)))
        (message "Nothing to evaluate at point")
      (let* ((element (org-element-at-point))
             (block-beg (save-excursion
                          (goto-char (org-babel-where-is-src-block-head element))
                          (line-beginning-position 2)))
             (block-end (save-excursion
                          (goto-char (org-element-property :end element))
                          (skip-chars-backward " \t\n")
                          (line-beginning-position)))
             (beg (if beg (max beg block-beg) block-beg))
             (end (if end (min end block-end) block-end))
             (lang (or (org-eldoc-get-src-lang)
                       (user-error "No lang specified for this src block"))))
        (cond ((and (string-prefix-p "jupyter-" lang)
                    (require 'jupyter nil t))
               (jupyter-eval-region beg end))
              ((+eval-region-as-major-mode beg end (org-src-get-lang-mode lang))))))))


;;;###autoload
(defun +org-lookup-definition-handler (identifier)
  "TODO"
  (when (org-in-src-block-p t)
    (let ((mode (org-src-get-lang-mode
                 (or (org-eldoc-get-src-lang)
                     (user-error "No lang specified for this src block")))))
      (cond ((and (eq mode 'emacs-lisp-mode)
                  (fboundp '+emacs-lisp-lookup-definition))
             (+emacs-lisp-lookup-definition identifier)
             'deferred)
            ((user-error "Definition lookup in SRC blocks isn't supported yet"))))))

;;;###autoload
(defun +org-lookup-references-handler (_identifier)
  "TODO"
  (when (org-in-src-block-p t)
    (user-error "References lookup in SRC blocks isn't supported yet")))

;;;###autoload
(defun +org-lookup-documentation-handler (identifier)
  "TODO"
  (when (org-in-src-block-p t)
    (let ((mode (org-src-get-lang-mode
                 (or (org-eldoc-get-src-lang)
                     (user-error "No lang specified for this src block"))))
          (info (org-babel-get-src-block-info t)))
      (cond ((string-prefix-p "jupyter-" (car info))
             (and (require 'jupyter nil t)
                  (call-interactively #'jupyter-inspect-at-point)
                  (display-buffer (help-buffer))
                  'deferred))
            ((and (eq mode 'emacs-lisp-mode)
                  (fboundp '+emacs-lisp-lookup-documentation))
             (+emacs-lisp-lookup-documentation identifier)
             'deferred)
            ((user-error "Documentation lookup in SRC blocks isn't supported yet"))))))


;;
;;; Commands

;;;###autoload
(defun +org/remove-result-blocks (remove-all)
  "Remove all result blocks located after current point."
  (interactive "P")
  (let ((pos (point)))
    (org-babel-map-src-blocks nil
      (if (or remove-all (< pos end-block))
          (org-babel-remove-result)))))


;;
;;; Hooks

;;;###autoload
(defun +org-clear-babel-results-h ()
  "Remove the results block for the org babel block at point."
  (when (and (org-in-src-block-p t)
             (org-babel-where-is-src-block-result))
    (org-babel-remove-result)
    t))
