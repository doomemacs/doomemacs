;;; completion/helm/autoload/helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +helm/tasks (&optional _arg)
  (interactive "P")
  ;; TODO Implement `+helm/tasks'
  (error "Not implemented yet"))

;;;###autoload
(defun +helm/projectile-find-file ()
  "Call `helm-find-files' if called from HOME, otherwise
`helm-projectile-find-file'."
  (interactive)
  (call-interactively
   (if (or (file-equal-p default-directory "~")
           (if-let* ((proot (doom-project-root)))
               (file-equal-p proot "~")
             t))
       #'helm-find-files
     #'helm-projectile-find-file)))

;;;###autoload
(defun +helm/workspace-buffer-list ()
  "A version of `helm-buffers-list' with its buffer list restricted to the
current workspace."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error "This command requires the :feature workspaces module"))
  (with-no-warnings
    (with-persp-buffer-list nil (helm-buffers-list))))

;;;###autoload
(defun +helm/workspace-mini ()
  "A version of `helm-mini' with its buffer list restricted to the current
workspace."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error "This command requires the :feature workspaces module"))
  (with-no-warnings
    (with-persp-buffer-list nil (helm-mini))))


;;
;; Project search

(defun +helm-ag-search-args (all-files-p recursive-p)
  (list (concat "ag " (if IS-WINDOWS "--vimgrep" "--nocolor --nogroup"))
        "-S"
        (if all-files-p "-z -a")
        (unless recursive-p "--depth 1")))

(defun +helm-rg-search-args (all-files-p recursive-p)
  (list "rg --no-heading --line-number --color never"
        "-S"
        (when all-files-p "-z -uu")
        (unless recursive-p "--maxdepth 1")))

(defun +helm-pt-search-args (all-files-p recursive-p)
  (list "pt --nocolor --nogroup -e"
        "-S"
        (if all-files-p "-z -a")
        (unless recursive-p "--depth 1")))

;;
(defun +helm--grep-source ()
  (require 'helm-projectile)
  (helm-build-async-source (capitalize (helm-grep-command t))
    :header-name (lambda (_name) "Helm Projectile Grep (C-c ? Help)")
    :candidates-process #'helm-grep-collect-candidates
    :filter-one-by-one #'helm-grep-filter-one-by-one
    :candidate-number-limit 9999
    :nohighlight t
    :keymap helm-grep-map
    :history 'helm-grep-history
    :action (apply #'helm-make-actions helm-projectile-grep-or-ack-actions)
    :persistent-action 'helm-grep-persistent-action
    :persistent-help "Jump to line (`C-u' Record in mark ring)"
    :requires-pattern 2))

(defun +helm--grep-search (directory query prompt &optional all-files-p recursive-p)
  (let* ((default-directory directory)
         (helm-ff-default-directory directory)
         (helm-grep-in-recurse recursive-p)
         (helm-grep-ignored-files
          (unless all-files-p
            (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)))
         (helm-grep-ignored-directories
          (unless all-files-p
            (cl-union (mapcar 'directory-file-name (projectile-ignored-directories-rel))
                      grep-find-ignored-directories)))
         (helm-grep-default-command
          (if (and nil (eq (projectile-project-vcs) 'git))
              (format "git --no-pager grep --no-color -n%%c -e %%p %s -- %%f"
                      (if recursive-p "" "--max-depth 1 "))
            (format "grep -si -a%s %%e -n%%cH -e %%p %%f %s"
                    (if recursive-p " -R" "")
                    (if recursive-p "." "./*"))))
         (helm-grep-default-recurse-command helm-grep-default-command))
    (setq helm-source-grep (+helm--grep-source))
    (helm :sources 'helm-source-grep
          :input query
          :prompt prompt
          :buffer "*helm grep*"
          :default-directory directory
          :keymap helm-grep-map
          :history 'helm-grep-history
          :truncate-lines helm-grep-truncate-lines)))

;;;###autoload
(cl-defun +helm-file-search (engine &key query in all-files (recursive t))
  "Conduct a file search using ENGINE, which can be any of: rg, ag, pt, and
grep. If omitted, ENGINE will default to the first one it detects, in that
order.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (require 'helm-ag)
  (helm-ag--init-state)
  (let* ((project-root (or (doom-project-root) default-directory))
         (directory (or in project-root))
         (default-directory directory)
         (helm-ag--default-directory directory)
         (helm-ag--default-target (list directory))
         (engine (or engine
                     (cl-find-if #'executable-find +helm-project-search-engines
                                 :key #'symbol-name)
                     (and (or (executable-find "grep")
                              (executable-find "git"))
                          'grep)
                     (user-error "No search engine specified (is ag, rg, pt or git installed?)")))
         (query (or query
                    (when (use-region-p)
                      (let ((beg (or (bound-and-true-p evil-visual-beginning) (region-beginning)))
                            (end (or (bound-and-true-p evil-visual-end) (region-end))))
                        (when (> (abs (- end beg)) 1)
                          (rxt-quote-pcre (buffer-substring-no-properties beg end)))))
                    ""))
         (prompt (format "[%s %s] "
                         (symbol-name engine)
                         (cond ((file-equal-p directory project-root)
                                (projectile-project-name))
                               ((file-equal-p directory default-directory)
                                "./")
                               ((file-relative-name directory project-root)))))
         (command
          (pcase engine
            (`ag   (+helm-ag-search-args all-files recursive))
            (`rg   (+helm-rg-search-args all-files recursive))
            (`pt   (+helm-pt-search-args all-files recursive))
            ('grep (+helm--grep-search directory query prompt all-files recursive)
                   (cl-return t))))
         (helm-ag-base-command (string-join command " ")))
    ;; TODO Define our own sources instead
    (helm-attrset 'name (format "[%s %s] Searching %s"
                                engine
                                (string-join (delq nil (cdr command)) " ")
                                (abbreviate-file-name directory))
                  helm-source-do-ag)
    (cl-letf ((+helm-global-prompt prompt)
              ((symbol-function 'helm-do-ag--helm)
               (lambda () (helm :sources '(helm-source-do-ag)
                           :buffer "*helm-ag*"
                           :keymap helm-do-ag-map
                           :input query
                           :history 'helm-ag--helm-history))))
      (helm-do-ag directory))))

(defun +helm--get-command (format)
  (cl-loop for tool in (cl-remove-duplicates +helm-project-search-engines :from-end t)
           if (executable-find (symbol-name tool))
           return (intern (format format tool))))

;;;###autoload
(defun +helm/project-search (&optional all-files-p)
  "Performs a project search from the project root.

Uses the first available search backend from `+helm-project-search-engines'. If
ALL-FILES-P (universal argument), include all files, even hidden or compressed
ones, in the search."
  (interactive "P")
  (funcall (or (+helm--get-command "+helm/%s")
               #'+helm/grep)
           (or all-files-p current-prefix-arg)))

;;;###autoload
(defun +helm/project-search-from-cwd (&optional all-files-p)
  "Performs a project search recursively from the current directory.

Uses the first available search backend from `+helm-project-search-engines'. If
ALL-FILES-P (universal argument), include all files, even hidden or compressed
ones."
  (interactive "P")
  (funcall (or (+helm--get-command "+helm/%s-from-cwd")
               #'+helm/grep-from-cwd)
           (or all-files-p current-prefix-arg)))


;; Relative to project root
;;;###autoload (autoload '+helm/rg "completion/helm/autoload/helm")
;;;###autoload (autoload '+helm/rg-from-cwd "completion/helm/autoload/helm")
;;;###autoload (autoload '+helm/ag "completion/helm/autoload/helm")
;;;###autoload (autoload '+helm/ag-from-cwd "completion/helm/autoload/helm")
;;;###autoload (autoload '+helm/pt "completion/helm/autoload/helm")
;;;###autoload (autoload '+helm/pt-from-cwd "completion/helm/autoload/helm")
;;;###autoload (autoload '+helm/grep "completion/helm/autoload/helm")
;;;###autoload (autoload '+helm/grep-from-cwd "completion/helm/autoload/helm")

(dolist (engine `(,@(cl-remove-duplicates +helm-project-search-engines :from-end t) grep))
  (defalias (intern (format "+helm/%s" engine))
    (lambda (all-files-p &optional query directory)
      (interactive "P")
      (+helm-file-search engine :query query :in directory :all-files all-files-p))
    (format "Perform a project file search using %s.

QUERY is a regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, search compressed and hidden files as well."
            engine))

  (defalias (intern (format "+helm/%s-from-cwd" engine))
    (lambda (all-files-p &optional query)
      (interactive "P")
      (+helm-file-search engine :query query :in default-directory :all-files all-files-p))
    (format "Perform a project file search from the current directory using %s.

QUERY is a regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, search compressed and hidden files as well."
            engine)))
