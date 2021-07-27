;;; completion/vertico/autoload/vertico.el -*- lexical-binding: t; -*-

;; To prevent "Unused lexical variable" warning from +vertico--company-capf--candidates-a
;;;###autoload
(defvar orderless-match-faces)

;;;###autoload
(defadvice! +vertico--company-capf--candidates-a (fn &rest args)
  "Highlight company matches correctly, and try default completion styles before
orderless."
  :around 'company-capf--candidates
  (let ((orderless-match-faces [completions-common-part])
        (completion-styles +vertico-company-completion-styles))
    (apply fn args)))

;;;###autoload
(cl-defun +vertico-file-search (&key query in all-files (recursive t) prompt args)
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
  (let* ((project-root (or (doom-project-root) default-directory))
         (directory (or in project-root))
         (args
          (split-string
           (string-trim
            (concat (if all-files "-uu")
                    (unless recursive "--maxdepth 1")
                    "--null --line-buffered --color=always --max-columns=500 --no-heading --line-number"
                    " --hidden -g !.git "
                    (mapconcat #'shell-quote-argument args " ")))
           " "))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query (or query
                    (when (doom-region-active-p)
                      (rxt-quote-pcre (doom-thing-at-point-or-region)))))
         (ripgrep-command (string-join `("rg" ,@args "." "-e ARG OPTS" ) " "))
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" separator)
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt ripgrep-command directory query)))

;;;###autoload
(defun +vertico/project-search (&optional arg initial-query directory)
  "Peforms a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +vertico/project-search-from-cwd (&optional arg initial-query)
  "Performs a live project search from the current directory.
If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+vertico/project-search arg initial-query default-directory))

;;;###autoload
(defun +vertico/search-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;###autoload
(defun +vertico/backward-updir ()
  "Delete char before or go up directory for file cagetory vertico buffers."
  (interactive)
  (let ((metadata (completion-metadata (minibuffer-contents)
                                       minibuffer-completion-table
                                       minibuffer-completion-predicate)))
    (if (and (eq (char-before) ?/)
             (eq (completion-metadata-get metadata 'category) 'file))
        (let ((new-path (minibuffer-contents)))
          (delete-region (minibuffer-prompt-end) (point-max))
          (insert (abbreviate-file-name
                   (file-name-directory
                    (directory-file-name
                     (expand-file-name new-path))))))
      (call-interactively 'backward-delete-char))))

(defun +vertico--embark-target-package ()
    "Targets Doom's package! statements and returns the package name"
    (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'org-mode))
      (save-excursion
        (search-backward "(")
        (when (looking-at "(\\s-*package!\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")
          (let ((pkg (match-string 1)))
            (set-text-properties 0 (length pkg) nil pkg)
            `(package . ,pkg))))))

;;;###autoload
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))))

;;;###autoload
(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-default-action)))))

;;;###autoload
(defun +vertico/next-candidate-preview (&optional n)
  "Go forward N candidates and preivew"
  (interactive)
  (vertico-next (or n 1))
  (+vertico/embark-preview))

;;;###autoload
(defun +vertico/previous-candidate-preview (&optional n)
  "Go backward N candidates and preivew"
  (interactive)
  (vertico-previous (or n 1))
  (+vertico/embark-preview))
