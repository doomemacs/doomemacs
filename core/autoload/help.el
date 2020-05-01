;;; core/autoload/help.el -*- lexical-binding: t; -*-

(defvar doom--help-major-mode-module-alist
  '((dockerfile-mode :tools docker)
    (agda2-mode      :lang agda)
    (c-mode          :lang cc)
    (c++-mode        :lang cc)
    (objc++-mode     :lang cc)
    (crystal-mode    :lang crystal)
    (lisp-mode       :lang common-lisp)
    (csharp-mode     :lang csharp)
    (clojure-mode    :lang clojure)
    (clojurescript-mode :lang clojure)
    (graphql-mode    :lang data)
    (toml-mode       :lang data)
    (json-mode       :lang data)
    (yaml-mode       :lang data)
    (csv-mode        :lang data)
    (dhall-mode      :lang data)
    (erlang-mode     :lang erlang)
    (elixir-mode     :lang elixir)
    (elm-mode        :lang elm)
    (emacs-lisp-mode :lang emacs-lisp)
    (ess-r-mode      :lang ess)
    (ess-julia-mode  :lang ess)
    (go-mode         :lang go)
    (haskell-mode    :lang haskell)
    (hy-mode         :lang hy)
    (idris-mode      :lang idris)
    (java-mode       :lang java)
    (js2-mode        :lang javascript)
    (rjsx-mode       :lang javascript)
    (typescript-mode :lang javascript)
    (coffee-mode     :lang javascript)
    (julia-mode      :lang julia)
    (kotlin-mode     :lang kotlin)
    (latex-mode      :lang latex)
    (LaTeX-mode      :lang latex)
    (ledger-mode     :lang ledger)
    (lua-mode        :lang lua)
    (moonscript-mode :lang lua)
    (markdown-mode   :lang markdown)
    (gfm-mode        :lang markdown)
    (nim-mode        :lang nim)
    (nix-mode        :lang nix)
    (taureg-mode     :lang ocaml)
    (org-mode        :lang org)
    (perl-mode       :lang perl)
    (raku-mode       :lang perl)
    (php-mode        :lang php)
    (hack-mode       :lang php)
    (plantuml-mode   :lang plantuml)
    (purescript-mode :lang purescript)
    (python-mode     :lang python)
    (restclient-mode :lang rest)
    (ruby-mode       :lang ruby)
    (rust-mode       :lang rust)
    (rustic-mode     :lang rust)
    (scala-mode      :lang scala)
    (scheme-mode     :lang scheme)
    (sh-mode         :lang sh)
    (swift-mode      :lang swift)
    (web-mode        :lang web)
    (css-mode        :lang web)
    (scss-mode       :lang web)
    (sass-mode       :lang web)
    (less-css-mode   :lang web)
    (stylus-mode     :lang web)
    (terra-mode      :lang terra))
  "An alist mapping major modes to Doom modules.

This is used by `doom/help-modules' to auto-select the module corresponding to
the current major-modea.")


;;
;;; Helpers

;;;###autoload
(defun doom-active-minor-modes ()
  "Return a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           if (and (boundp mode) (symbol-value mode))
           collect mode))


;;
;;; Custom describe commands

;;;###autoload (defalias 'doom/describe-autodefs #'doom/help-autodefs)
;;;###autoload (defalias 'doom/describe-module   #'doom/help-modules)
;;;###autoload (defalias 'doom/describe-package  #'doom/help-packages)

;;;###autoload
(defun doom/describe-active-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Describe active mode: " (doom-active-minor-modes))))
  (let ((symbol
         (cond ((stringp mode) (intern mode))
               ((symbolp mode) mode)
               ((error "Expected a symbol/string, got a %s" (type-of mode))))))
    (if (fboundp symbol)
        (helpful-function symbol)
      (helpful-variable symbol))))


;;
;;; Documentation commands

(defvar org-agenda-files)
(defun doom--org-headings (files &optional depth include-files)
  "TODO"
  (require 'org)
  (let* ((default-directory doom-docs-dir)
         (org-agenda-files (mapcar #'expand-file-name (doom-enlist files)))
         (depth (if (integerp depth) depth)))
    (message "Loading search results...")
    (unwind-protect
        (delq
         nil
         (org-map-entries
          (lambda ()
            (cl-destructuring-bind (level _reduced-level _todo _priority text tags)
                (org-heading-components)
              (when (and (or (null depth)
                             (<= level depth))
                         (or (null tags)
                             (not (string-match-p ":TOC" tags))))
                (let ((path (org-get-outline-path)))
                  (list (string-join
                         (list (string-join
                                (append (when include-files
                                          (list (or (+org-get-global-property "TITLE")
                                                    (file-relative-name (buffer-file-name)))))
                                        path
                                        (list (replace-regexp-in-string org-link-any-re "\\4" text)))
                                " > ")
                               tags)
                         " ")
                        (buffer-file-name)
                        (point))))))
          t 'agenda))
      (mapc #'kill-buffer org-agenda-new-buffers)
      (setq org-agenda-new-buffers nil))))

(defvar ivy-sort-functions-alist)
;;;###autoload
(defun doom-completing-read-org-headings (prompt files &optional depth include-files initial-input extra-candidates)
  "TODO"
  (let ((alist
         (append (doom--org-headings files depth include-files)
                 extra-candidates))
        ivy-sort-functions-alist)
    (if-let (result (completing-read prompt alist nil nil initial-input))
        (cl-destructuring-bind (file &optional location)
            (cdr (assoc result alist))
          (find-file file)
          (cond ((functionp location)
                 (funcall location))
                (location
                 (goto-char location)))
          (ignore-errors
            (when (outline-invisible-p)
              (save-excursion
                (outline-previous-visible-heading 1)
                (org-show-subtree)))))
      (user-error "Aborted"))))

;;;###autoload
(defun doom/homepage ()
  "Open the doom emacs homepage in the browser."
  (interactive)
  (browse-url "https://doomemacs.org"))

;;;###autoload
(defun doom/help ()
  "Open Doom's user manual."
  (interactive)
  (find-file (expand-file-name "index.org" doom-docs-dir)))

;;;###autoload
(defun doom/help-search-headings (&optional initial-input)
  "Search Doom's documentation and jump to a headline."
  (interactive)
  (doom-completing-read-org-headings
   "Find in Doom help: "
   (list "getting_started.org"
         "contributing.org"
         "troubleshooting.org"
         "tutorials.org"
         "faq.org")
   2 t initial-input
   (mapcar (lambda (x)
             (setcar x (concat "Doom Modules > " (car x)))
             x)
           (doom--help-modules-list))))

;;;###autoload
(defun doom/help-search (&optional initial-input)
  "Preform a text search on all of Doom's documentation."
  (interactive)
  (funcall (cond ((fboundp '+ivy-file-search)
                  #'+ivy-file-search)
                 ((fboundp '+helm-file-search)
                  #'+helm-file-search)
                 ((rgrep
                   (read-regexp
                    "Search for" (or initial-input 'grep-tag-default)
                    'grep-regexp-history)
                   "*.org" doom-emacs-dir)
                  #'ignore))
           :query initial-input
           :args '("-g" "*.org")
           :in doom-emacs-dir
           :prompt "Search documentation for: "))

;;;###autoload
(defun doom/help-news-search (&optional initial-input)
  "Search headlines in Doom's newsletters."
  (interactive)
  (doom-completing-read-org-headings
   "Find in News: "
   (nreverse (doom-files-in (expand-file-name "news" doom-docs-dir)
                            :match "/[0-9]"
                            :relative-to doom-docs-dir))
   nil t initial-input))

;;;###autoload
(defun doom/help-faq (&optional initial-input)
  "Search Doom's FAQ and jump to a question."
  (interactive)
  (doom-completing-read-org-headings
   "Find in FAQ: " (list "faq.org")
   2 nil initial-input))

;;;###autoload
(defun doom/help-news ()
  "Open a Doom newsletter.
The latest newsletter will be selected by default."
  (interactive)
  (let* ((default-directory (expand-file-name "news/" doom-docs-dir))
         (news-files (doom-files-in default-directory)))
    (find-file
     (read-file-name (format "Open Doom newsletter (current: v%s): "
                             doom-version)
                     default-directory
                     (if (member doom-version news-files)
                         doom-version
                       (concat (mapconcat #'number-to-string
                                          (nbutlast (version-to-list doom-version) 1)
                                          ".")
                               ".x"))
                     t doom-version))))

;;;###autoload
(defun doom/help-autodefs (autodef)
  "Open documentation for an autodef.

An autodef is a Doom concept. It is a function or macro that is always defined,
whether or not its containing module is disabled (in which case it will safely
no-op without evaluating its arguments). This syntactic sugar lets you use them
without needing to check if they are available."
  (interactive
   (let* ((settings
           (cl-loop with case-fold-search = nil
                    for sym being the symbols of obarray
                    for sym-name = (symbol-name sym)
                    if (and (or (functionp sym)
                                (macrop sym))
                            (string-match-p "[a-z]!$" sym-name))
                    collect sym))
          (sym (symbol-at-point))
          (autodef
           (completing-read
            "Describe setter: "
            ;; TODO Could be cleaner (refactor me!)
            (cl-loop with maxwidth = (apply #'max (mapcar #'length (mapcar #'symbol-name settings)))
                     for def in (sort settings #'string-lessp)
                     if (get def 'doom-module)
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "%s %s" (car it) (cdr it))
                                             'face 'font-lock-comment-face))
                     else if (and (string-match-p "^set-.+!$" (symbol-name def))
                                  (symbol-file def)
                                  (file-in-directory-p (symbol-file def) doom-core-dir))
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "core/%s.el" (file-name-sans-extension (file-relative-name (symbol-file def) doom-core-dir)))
                                             'face 'font-lock-comment-face)))
            nil t
            (when (and (symbolp sym)
                       (string-match-p "!$" (symbol-name sym)))
              (symbol-name sym)))))
     (list (and autodef (car (split-string autodef " "))))))
  (or (stringp autodef)
      (functionp autodef)
      (signal 'wrong-type-argument (list '(stringp functionp) autodef)))
  (let ((fn (if (functionp autodef)
                autodef
              (intern-soft autodef))))
    (or (fboundp fn)
        (error "'%s' is not a valid DOOM autodef" autodef))
    (if (fboundp 'helpful-callable)
        (helpful-callable fn)
      (describe-function fn))))

(defun doom--help-modules-list ()
  (cl-loop for path in (cdr (doom-module-load-path 'all))
           for (cat . mod) = (doom-module-from-path path)
           for readme-path = (or (doom-module-locate-path cat mod "README.org")
                                 (doom-module-locate-path cat mod))
           for format = (format "%s %s" cat mod)
           if (doom-module-p cat mod)
           collect (list format readme-path)
           else if (and cat mod)
           collect (list (propertize format 'face 'font-lock-comment-face)
                         readme-path)))

(defun doom--help-current-module-str ()
  (cond ((save-excursion
           (require 'smartparens)
           (ignore-errors
             (sp-beginning-of-sexp)
             (unless (eq (char-after) ?\()
               (backward-char))
             (let ((sexp (sexp-at-point)))
               (when (memq (car-safe sexp) '(featurep! require!))
                 (format "%s %s" (nth 1 sexp) (nth 2 sexp)))))))
        ((when buffer-file-name
           (when-let (mod (doom-module-from-path buffer-file-name))
             (unless (memq (car mod) '(:core :private))
               (format "%s %s" (car mod) (cdr mod))))))
        ((when-let (mod (cdr (assq major-mode doom--help-major-mode-module-alist)))
           (format "%s %s"
                   (symbol-name (car mod))
                   (symbol-name (cadr mod)))))))

;;;###autoload
(defun doom/help-modules (category module &optional visit-dir)
  "Open the documentation for a Doom module.

CATEGORY is a keyword and MODULE is a symbol. e.g. :editor and 'evil.

If VISIT-DIR is non-nil, visit the module's directory rather than its
documentation.

Automatically selects a) the module at point (in private init files), b) the
module derived from a `featurep!' or `require!' call, c) the module that the
current file is in, or d) the module associated with the current major mode (see
`doom--help-major-mode-module-alist')."
  (interactive
   (mapcar #'intern
           (split-string
            (completing-read "Describe module: "
                             (doom--help-modules-list)
                             nil t nil nil
                             (doom--help-current-module-str))
            " " t)))
  (cl-check-type category symbol)
  (cl-check-type module symbol)
  (cl-destructuring-bind (module-string path)
      (or (assoc (format "%s %s" category module) (doom--help-modules-list))
          (user-error "'%s %s' is not a valid module" category module))
    (setq module-string (substring-no-properties module-string))
    (unless (file-readable-p path)
      (error "Can't find or read %S module at %S" module-string path))
    (cond ((not (file-directory-p path))
           (if visit-dir
               (doom-project-browse (file-name-directory path))
             (find-file path)))
          (visit-dir
           (doom-project-browse path))
          ((y-or-n-p (format "The %S module has no README file. Explore its directory?"
                             module-string))
           (doom-project-browse (file-name-directory path)))
          ((user-error "Aborted module lookup")))))


;;
;;; `doom/help-packages'

(defun doom--help-package-insert-button (label path &optional regexp)
  (declare (indent defun))
  (insert-text-button
   (string-trim label)
   'face 'link
   'follow-link t
   'action
   `(lambda (_)
      (unless (file-exists-p ,path)
        (user-error "Module doesn't exist"))
      (when (window-dedicated-p)
        (other-window 1))
      (let ((buffer (find-file ,path)))
        (when ,(stringp regexp)
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (re-search-forward ,regexp nil t)
                (recenter)
              (message "Couldn't find the config block"))))))))

(defun doom--help-package-configs (package)
  (let ((default-directory doom-emacs-dir))
    ;; TODO Use ripgrep instead
    (split-string
     (cdr (doom-call-process
           "git" "grep" "--no-break" "--no-heading" "--line-number"
           (format "%s %s\\($\\| \\)"
                   "\\(^;;;###package\\|(after!\\|(use-package!\\)"
                   package)
           ":(exclude)*.org"))
     "\n" t)))

;;;###autoload
(defun doom/help-packages (package)
  "Like `describe-package', but for packages installed by Doom modules.

Only shows installed packages. Includes information about where packages are
defined and configured.

If prefix arg is present, refresh the cache."
  (interactive
   (let ((guess (or (function-called-at-point)
                    (symbol-at-point))))
     (require 'finder-inf nil t)
     (require 'core-packages)
     (doom-initialize-packages)
     (let ((packages (delete-dups
                      (append (mapcar #'car package-alist)
                              (mapcar #'car package--builtins)
                              (mapcar #'intern (hash-table-keys straight--build-cache))
                              (mapcar #'car (doom-package-list 'all))
                              nil))))
       (unless (memq guess packages)
         (setq guess nil))
       (list
        (intern
         (completing-read (if guess
                              (format "Select Doom package to search for (default %s): "
                                      guess)
                            "Describe Doom package: ")
                          packages nil t nil nil
                          (if guess (symbol-name guess))))))))
  (require 'core-packages)
  (doom-initialize-packages)
  (if (or (package-desc-p package)
          (and (symbolp package)
               (or (assq package package-alist)
                   (assq package package--builtins))))
      (describe-package package)
    (help-setup-xref (list #'doom/help-packages package)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)))
  (save-excursion
    (with-current-buffer (help-buffer)
      (let ((inhibit-read-only t)
            (indent (make-string 13 ? )))
        (goto-char (point-max))
        (if (re-search-forward "^ *Status: " nil t)
            (progn
              (end-of-line)
              (insert "\n"))
          (search-forward "\n\n" nil t))

        (package--print-help-section "Package")
        (insert (symbol-name package) "\n")

        (package--print-help-section "Source")
        (pcase (doom-package-backend package)
          (`straight
           (insert "Straight\n")
           (package--print-help-section "Pinned")
           (insert (if-let (pin (plist-get (cdr (assq package doom-packages)) :pin))
                       pin
                     "unpinned")
                   "\n")
           (package--print-help-section "Build")
           (insert (let ((default-directory (straight--repos-dir (symbol-name package))))
                     (cdr
                      (doom-call-process "git" "log" "-1" "--format=%D %h %ci")))
                   "\n")
           (let ((recipe (doom-package-build-recipe package)))
             (insert (format! "%s\n"
                              (indent 13
                                      (string-trim (pp-to-string recipe)))))

             (package--print-help-section "Homepage")
             (insert (doom--package-url package))))
          (`elpa (insert "[M]ELPA " (doom--package-url package)))
          (`builtin (insert "Built-in"))
          (`other (insert
                   (abbreviate-file-name
                    (or (symbol-file package)
                        (locate-library (symbol-name package))))))
          (_ (insert "Not installed")))
        (insert "\n")

        (when-let
            (modules
             (if (gethash (symbol-name package) straight--build-cache)
                 (doom-package-get package :modules)
               (plist-get (cdr (assq package (doom-packages-list 'all)))
                          :modules)))
          (package--print-help-section "Modules")
          (insert "Declared by the following Doom modules:\n")
          (dolist (m modules)
            (insert indent)
            (doom--help-package-insert-button
              (format "%s %s" (car m) (or (cdr m) ""))
              (pcase (car m)
                (:core doom-core-dir)
                (:private doom-private-dir)
                (category (doom-module-path category (cdr m)))))
            (insert "\n")))

        (package--print-help-section "Configs")
        (insert "This package is configured in the following locations:")
        (dolist (location (doom--help-package-configs package))
          (insert "\n" indent)
          (insert-text-button
           location
           'face 'link
           'follow-link t
           'action
           `(lambda (_)
              (cl-destructuring-bind (file line _match)
                  ',(split-string location ":")
                (find-file (expand-file-name file doom-emacs-dir))
                (goto-char (point-min))
                (forward-line (1- (string-to-number line)))
                (recenter)))))

        (insert "\n\n")))))

(defvar doom--package-cache nil)
(defun doom--package-list (&optional prompt)
  (let* ((guess (or (function-called-at-point)
                    (symbol-at-point))))
    (require 'finder-inf nil t)
    (unless package--initialized
      (package-initialize t))
    (let ((packages (or doom--package-cache
                        (progn
                          (message "Reading packages...")
                          (cl-delete-duplicates
                           (append (mapcar 'car package-alist)
                                   (mapcar 'car package--builtins)
                                   (mapcar 'car package-archive-contents)))))))
      (setq doom--package-cache packages)
      (unless (memq guess packages)
        (setq guess nil))
      (intern (completing-read (or prompt
                                   (if guess
                                       (format "Select package to search for (default %s): "
                                               guess)
                                     "Describe package: "))
                               packages nil t nil nil
                               (if guess (symbol-name guess)))))))

(defun doom--package-url (package)
  (cond ((assq package package--builtins)
         (user-error "Package is built into Emacs and cannot be looked up"))
        ((when-let (location (locate-library (symbol-name package)))
           (with-temp-buffer
             (insert-file-contents (concat (file-name-sans-extension location) ".el")
                                   nil 0 4096)
             (let ((case-fold-search t))
               (when (re-search-forward " \\(?:URL\\|homepage\\|Website\\): \\(http[^\n]+\\)\n" nil t)
                 (match-string-no-properties 1))))))
        ((and (ignore-errors (eq (doom-package-backend package) 'quelpa))
              (let* ((plist (cdr (doom-package-prop package :recipe)))
                     (fetcher (plist-get plist :fetcher)))
                (pcase fetcher
                  (`git (plist-get plist :url))
                  (`github (format "https://github.com/%s.git" (plist-get plist :repo)))
                  (`gitlab (format "https://gitlab.com/%s.git" (plist-get plist :repo)))
                  (`bitbucket (format "https://bitbucket.com/%s" (plist-get plist :repo)))
                  (`wiki (format "https://www.emacswiki.org/emacs/download/%s"
                                 (or (car-safe (doom-enlist (plist-get plist :files)))
                                     (format "%s.el" package))))
                  (_ (plist-get plist :url))))))
        ((and (require 'package nil t)
              (or package-archive-contents (doom-refresh-packages-maybe))
              (pcase (package-desc-archive (cadr (assq package package-archive-contents)))
                ("org" "https://orgmode.org")
                ((or "melpa" "melpa-mirror")
                 (format "https://melpa.org/#/%s" package))
                ("gnu"
                 (format "https://elpa.gnu.org/packages/%s.html" package))
                (archive
                 (if-let (src (cdr (assoc package package-archives)))
                     (format "%s" src)
                   (user-error "%S isn't installed through any known source (%s)"
                               package archive))))))
        ((user-error "Cannot find the homepage for %S" package))))

;;;###autoload
(defun doom/help-package-config (package)
  "Jump to any `use-package!', `after!' or ;;;###package block for PACKAGE.

This only searches `doom-emacs-dir' (typically ~/.emacs.d) and does not include
config blocks in your private config."
  (interactive (list (doom--package-list "Find package config: ")))
  (cl-destructuring-bind (file line _match)
      (split-string
       (completing-read
        "Jump to config: "
        (or (doom--help-package-configs package)
            (user-error "This package isn't configured by you or Doom")))
       ":")
    (find-file (expand-file-name file doom-emacs-dir))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

;;;###autoload
(defalias 'doom/help-package-homepage #'straight-visit-package-website)

(defun doom--help-search-prompt (prompt)
  (let ((query (doom-thing-at-point-or-region)))
    (if (featurep 'counsel)
        query
      (read-string prompt query 'git-grep query))))

(defvar counsel-rg-base-command)
(defun doom--help-search (dirs query prompt)
  ;; REVIEW Replace with deadgrep
  (unless (executable-find "rg")
    (user-error "Can't find ripgrep on your system"))
  (if (fboundp 'counsel-rg)
      (let ((counsel-rg-base-command
             (concat counsel-rg-base-command " "
                     (mapconcat #'shell-quote-argument dirs " "))))
        (counsel-rg query nil "-Lz" prompt))
    ;; TODO Add helm support?
    (grep-find
     (string-join
      (append (list "rg" "-L" "--search-zip" "--no-heading" "--color=never"
                    (shell-quote-argument query))
              (mapcar #'shell-quote-argument dirs))
      " "))))

;;;###autoload
(defun doom/help-search-load-path (query)
  "Perform a text search on your `load-path'.
Uses the symbol at point or the current selection, if available."
  (interactive
   (list (doom--help-search-prompt "Search load-path: ")))
  (doom--help-search (cl-remove-if-not #'file-directory-p load-path)
                     query "Search load-path: "))

;;;###autoload
(defun doom/help-search-loaded-files (query)
  "Perform a text search on your `load-path'.
Uses the symbol at point or the current selection, if available."
  (interactive
   (list (doom--help-search-prompt "Search loaded files: ")))
  (let ((paths (cl-loop for (file . _) in load-history
                        for filebase = (file-name-sans-extension file)
                        if (file-exists-p! (format "%s.el" filebase))
                        collect it)))
    (doom--help-search paths query "Search loaded files: ")))
