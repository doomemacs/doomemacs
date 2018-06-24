;;; completion/helm/autoload/helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +helm/tasks (&optional _arg)
  (interactive "P")
  ;; TODO Implement `+helm/tasks'
  (error "Not implemented yet"))


;;
;; Project search
;;

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
  (let* ((project-root (doom-project-root))
         (directory (or in project-root))
         (default-directory directory)
         (helm-ag--default-directory directory)
         (helm-ag--default-target (list directory))
         (engine (or engine
                     (and (executable-find "rg") 'rg)
                     (and (executable-find "ag") 'ag)
                     (and (executable-find "pt") 'pt)
                     (and (or (executable-find "grep")
                              (executable-find "git"))
                          'grep)
                     (error "No search engine specified (is ag, rg, pt or git installed?)")))
         (query (or query
                    (when (use-region-p)
                      (let ((beg (or (bound-and-true-p evil-visual-beginning) (region-beginning)))
                            (end (or (bound-and-true-p evil-visual-end) (region-end))))
                        (when (> (abs (- end beg)) 1)
                          (rxt-quote-pcre (buffer-substring-no-properties beg end)))))
                    ""))
         (prompt (format "%s%%s %s"
                         (symbol-name engine)
                         (cond ((equal directory default-directory)
                                "./")
                               ((equal directory project-root)
                                (projectile-project-name))
                               (t
                                (file-relative-name directory project-root)))))
         (command
          (pcase engine
            ('grep
             (let* ((helm-ff-default-directory directory)
                    (helm-grep-in-recurse recursive)
                    (helm-grep-ignored-files
                     (unless all-files
                       (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)))
                    (helm-grep-ignored-directories
                     (unless all-files
                       (cl-union (mapcar 'directory-file-name (projectile-ignored-directories-rel))
                                 grep-find-ignored-directories)))
                    (helm-grep-default-command
                     (if (and nil (eq (projectile-project-vcs) 'git))
                         (format "git --no-pager grep --no-color -n%%c -e %%p %s -- %%f"
                                 (if recursive "" "--max-depth 1 "))
                       (format "grep -si -a%s %%e -n%%cH -e %%p %%f %s"
                               (if recursive " -R" "")
                               (if recursive "." "./*"))))
                    (helm-grep-default-recurse-command helm-grep-default-command))
               (setq helm-source-grep
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
               (helm :sources 'helm-source-grep
                     :input query
                     :prompt prompt
                     :buffer "*helm grep*"
                     :default-directory directory
                     :keymap helm-grep-map
                     :history 'helm-grep-history
                     :truncate-lines helm-grep-truncate-lines))
             (cl-return t))
            (`ag
             (list "ag -zS"
                   (if IS-WINDOWS "--vimgrep" "--nocolor --nogroup")
                   (when all-files "-a")
                   (unless recursive "--depth 1")))
            (`rg
             (list "rg -zS --no-heading --line-number --color never"
                   (when all-files "-uu")
                   (unless recursive "--maxdepth 1")))
            (`pt
             (list "pt -zS --nocolor --nogroup -e"
                   (when all-files "-a")
                   (unless recursive "--depth 1")))))
         (helm-ag-base-command (string-join command " ")))
    (if (and (eq engine 'ag)
             (equal query ""))
        (helm-do-ag directory)
      (setq helm-ag--last-query query)
      (helm-attrset 'search-this-file nil helm-ag-source)
      (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory) helm-ag-source)
      (helm :sources '(helm-ag-source)
            :input query
            :prompt prompt
            :buffer "*helm-ag*"
            :keymap helm-ag-map
            :history 'helm-ag--helm-history))))

;;;###autoload
(defun +helm/project-search (arg)
  "Performs a project search using the first available search backend from a
list of: ripgrep, ag, pt, git-grep and grep. If ARG (universal argument),
preform search from current directory."
  (interactive "P")
  (call-interactively
   (cond ((executable-find "rg") (if arg #'+helm/rg-from-cwd #'+helm/rg))
         ((executable-find "ag") (if arg #'+helm/ag-from-cwd #'+helm/ag))
         ((executable-find "pt") (if arg #'+helm/pt-from-cwd #'+helm/pt))
         (arg #'+helm/grep-from-cwd)
         (#'+helm/grep))))

;; Relative to project root
;;;###autoload
(defun +helm/rg (all-files-p &optional query directory)
  "TODO"
  (interactive "P")
  (+helm-file-search 'rg :query query :in directory :all-files all-files-p))

;;;###autoload
(defun +helm/ag (all-files-p &optional query directory)
  "TODO"
  (interactive "P")
  (+helm-file-search 'ag :query query :in directory :all-files all-files-p))

;;;###autoload
(defun +helm/pt (all-files-p &optional query directory)
  "TODO"
  (interactive "P")
  (+helm-file-search 'pt :query query :in directory :all-files all-files-p))

;;;###autoload
(defun +helm/grep (all-files-p &optional query directory)
  "TODO"
  (interactive "P")
  (+helm-file-search 'grep :query query :in directory :all-files all-files-p))

;; Relative to current directory
;;;###autoload
(defun +helm/rg-from-cwd (recurse-p &optional query)
  "TODO"
  (interactive "P")
  (+helm-file-search 'rg :query query :in default-directory :recursive recurse-p))

;;;###autoload
(defun +helm/ag-from-cwd (recurse-p &optional query)
  "TODO"
  (interactive "P")
  (+helm-file-search 'ag :query query :in default-directory :recursive recurse-p))

;;;###autoload
(defun +helm/pt-from-cwd (recurse-p &optional query)
  "TODO"
  (interactive "P")
  (+helm-file-search 'pt :query query :in default-directory :recursive recurse-p))

;;;###autoload
(defun +helm/grep-from-cwd (recurse-p &optional query)
  "TODO"
  (interactive "P")
  (+helm-file-search 'grep :query query :in default-directory :recursive recurse-p))

