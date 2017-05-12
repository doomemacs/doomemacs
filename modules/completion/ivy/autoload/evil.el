;;; completion/ivy/autoload/evil.el

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

(defun +ivy--file-search (engine beg end query &optional directory prompt)
  (let* ((directory (or directory (doom-project-root)))
         (recursion-p +ivy--file-search-recursion-p)
         (all-files-p +ivy--file-search-all-files-p)
         (project-root (doom-project-root))
         (query
          (or query
              (and (evil-visual-state-p)
                   beg end
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
    (cond ((eq engine 'ag)
           (let ((args (concat
                        (if all-files-p " -a")
                        (unless recursion-p " -n"))))
             (counsel-ag query directory args (format prompt args))))

          ((eq engine 'rg)
           ;; smart-case instead of case-insensitive flag
           (let ((counsel-rg-base-command
                  (replace-regexp-in-string " -i " " -S " counsel-rg-base-command))
                 (args (concat
                        (if all-files-p " -uu")
                        (unless recursion-p " --maxdepth 0"))))
             (counsel-rg query directory args (format prompt args))))

          ((eq engine 'pt)) ; TODO pt search engine (necessary?)

          (t (error "No search engine specified")))))

;;;###autoload (autoload '+ivy:ag "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag (beg end query &optional all-files-p directory)
  "Perform a project file search using the silver search. QUERY is a pcre
regexp. If omitted, will perform the last known search.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'ag beg end query directory)))

;;;###autoload (autoload '+ivy:rg "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:rg (beg end query &optional all-files-p directory)
  "Perform a project file search using ripgrep. QUERY is a regexp. If omitted,
will perform the last known search.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'rg beg end query directory)))


;;;###autoload (autoload '+ivy:ag-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag-cwd (beg end query &optional inhibit-recursion-p)
  "The same as :ag, but only searches the current directory. If
INHIBIT-RECURSION-P, don't recurse into sub-directories."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not inhibit-recursion-p)))
    (+ivy:ag beg end query t default-directory)))

;;;###autoload (autoload '+ivy:rg-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:rg-cwd (beg end query &optional inhibit-recursion-p)
  "The same as :rg, but only searches the current directory. If
INHIBIT-RECURSION-P, don't recurse into sub-directories."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not inhibit-recursion-p)))
    (+ivy:rg beg end query t default-directory)))
