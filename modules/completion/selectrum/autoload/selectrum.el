;;; completion/selectrum/autoload/selectrum.el -*- lexical-binding: t; -*-

;;;###autoload
(defadvice! +selectrum--company-capf--candidates-a (fn &rest args)
  "Function to help company to highlight all candidates with just
one face."
  :around 'company-capf--candidates
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

;;;###autoload
(cl-defun +selectrum-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (consult-ripgrep))

;;;###autoload
(defun +selectrum/project-search (&optional arg initial-query directory)
  "Peforms a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+selectrum-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +selectrum/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+selectrum/project-search arg initial-query default-directory))

;;;###autoload
(defun +selectrum/search-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))
