;;; completion/ivy/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+ivy:swiper "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper search))

;;;###autoload (autoload '+ivy:todo "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:todo (&optional bang)
  "An ex wrapper around `+ivy/tasks'."
  (interactive "<!>")
  (+ivy/tasks bang))


;; --- file searching ---------------------

(defvar +ivy--file-last-search nil)
(defvar +ivy--file-search-recursion-p t)
(defvar +ivy--file-search-all-files-p nil)

(defun +ivy--file-search (engine beg end query &optional directory)
  (let* ((project-root (doom-project-root))
         (directory (or directory project-root))
         (recursion-p +ivy--file-search-recursion-p)
         (all-files-p +ivy--file-search-all-files-p)
         (query
          (or query
              (and beg end
                   (> (abs (- end beg)) 1)
                   (rxt-quote-pcre (buffer-substring-no-properties beg end)))
              +ivy--file-last-search))
         (prompt
          (format "%s%%s %s"
                  (symbol-name engine)
                  (cond ((equal directory default-directory)
                         "./")
                        ((equal directory project-root)
                         (projectile-project-name))
                        (t
                         (file-relative-name directory project-root))))))
    (setq +ivy--file-last-search query)
    (pcase engine
      ('ag
       (let ((args (concat
                    (if all-files-p " -a")
                    (unless recursion-p " -n"))))
         (counsel-ag query directory args (format prompt args))))
      ('rg
       ;; smart-case instead of case-insensitive flag
       (let ((counsel-rg-base-command
              (replace-regexp-in-string " -i " " -S " counsel-rg-base-command))
             (args (concat
                    (if all-files-p " -uu")
                    (unless recursion-p " --maxdepth 0"))))
         (counsel-rg query directory args (format prompt args))))
      ('pt) ;; TODO pt search engine (necessary?)
      (_ (error "No search engine specified")))))

;;;###autoload (autoload '+ivy:ag "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag (beg end query &optional all-files-p directory)
  "Perform a project file search using the silver search. QUERY is a pcre
regexp. If omitted, the current selection is used. If no selection is active,
the last known search is used.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'ag beg end query directory)))

;;;###autoload (autoload '+ivy:rg "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:rg (beg end query &optional all-files-p directory)
  "Perform a project file search using ripgrep. QUERY is a regexp. If omitted,
the current selection is used. If no selection is active, the last known search
is used.

If ALL-FILES-P, don't respect .gitignore files and search everything.

NOTE: ripgrep doesn't support multiline searches (yet)."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'rg beg end query directory)))


;;;###autoload (autoload '+ivy:ag-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag-cwd (beg end query &optional bang)
  "The same as :ag, but searches the current directory. If BANG, don't recurse
into sub-directories."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not bang)))
    (+ivy:ag beg end query t default-directory)))

;;;###autoload (autoload '+ivy:rg-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:rg-cwd (beg end query &optional bang)
  "The same as :rg, but only searches the current directory. If BANG, don't
recurse into sub-directories.

NOTE: ripgrep doesn't support multiline searches (yet)."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not bang)))
    (+ivy:rg beg end query t default-directory)))
