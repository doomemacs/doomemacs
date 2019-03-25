;;; core/autoload/help.el -*- lexical-binding: t; -*-

(defvar doom--module-mode-alist
  '((dockerfile-mode :tools docker)
    (haxor-mode      :lang assembly)
    (mips-mode       :lang assembly)
    (nasm-mode       :lang assembly)
    (c-mode          :lang cc)
    (c++-mode        :lang cc)
    (objc++-mode     :lang cc)
    (crystal-mode    :lang crystal)
    (lisp-mode       :lang common-lisp)
    (csharp-mode     :lang csharp)
    (clojure-mode    :lang clojure)
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
    (java-mode       :lang java)
    (js2-mode        :lang javascript)
    (rjsx-mode       :lang javascript)
    (typescript-mode :lang javascript)
    (coffee-mode     :lang javascript)
    (julia-mode      :lang julia)
    (latex-mode      :lang latex)
    (LaTeX-mode      :lang latex)
    (ledger-mode     :lang ledger)
    (lua-mode        :lang lua)
    (markdown-mode   :lang markdown)
    (gfm-mode        :lang markdown)
    (nim-mode        :lang nim)
    (nix-mode        :lang nix)
    (taureg-mode     :lang ocaml)
    (org-mode        :lang org)
    (perl-mode       :lang perl)
    (php-mode        :lang php)
    (hack-mode       :lang php)
    (plantuml-mode   :lang plantuml)
    (purescript-mode :lang purescript)
    (python-mode     :lang python)
    (restclient-mode :lang rest)
    (ruby-mode       :lang ruby)
    (enh-ruby-mode   :lang ruby)
    (rust-mode       :lang rust)
    (scala-mode      :lang scala)
    (sh-mode         :lang sh)
    (swift-mode      :lang swift)
    (web-mode        :lang web)
    (css-mode        :lang web)
    (scss-mode       :lang web)
    (sass-mode       :lang web)
    (less-css-mode   :lang web)
    (stylus-mode     :lang web))
  "TODO")


;;
;; Helpers

;;;###autoload
(defun doom-active-minor-modes ()
  "Return a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           if (and (boundp mode) (symbol-value mode))
           collect mode))


;;
;; Commands

;;;###autoload
(defun doom/describe-autodefs (autodef)
  "Open the documentation of Doom autodefs.

What is an autodef? It's a function or macro that is always defined, even if its
containing module is disabled (in which case it will safely no-op). This
syntactic sugar lets you use them without needing to check if they are
available."
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

;;;###autoload
(defun doom/describe-active-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Minor mode: " (doom-active-minor-modes))))
  (describe-minor-mode-from-symbol
   (cond ((stringp mode) (intern mode))
         ((symbolp mode) mode)
         ((error "Expected a symbol/string, got a %s" (type-of mode))))))

;;;###autoload
(defun doom/describe-module (category module)
  "Open the documentation of CATEGORY MODULE.

CATEGORY is a keyword and MODULE is a symbol. e.g. :feature and 'evil.

Automatically selects a) the module at point (in private init files), b) the
module derived from a `featurep!' or `require!' call, c) the module that the
current file is in, or d) the module associated with the current major mode (see
`doom--module-mode-alist')."
  (interactive
   (let* ((module
           (cond ((and buffer-file-name
                       (eq major-mode 'emacs-lisp-mode)
                       (file-in-directory-p buffer-file-name doom-private-dir)
                       (save-excursion (goto-char (point-min))
                                       (re-search-forward "^\\s-*(doom! " nil t))
                       (thing-at-point 'sexp t)))
                 ((save-excursion
                    (require 'smartparens)
                    (ignore-errors
                      (sp-beginning-of-sexp)
                      (unless (eq (char-after) ?\()
                        (backward-char))
                      (let ((sexp (sexp-at-point)))
                        (when (memq (car-safe sexp) '(featurep! require!))
                          (format "%s %s" (nth 1 sexp) (nth 2 sexp)))))))
                 ((and buffer-file-name
                       (when-let* ((mod (doom-module-from-path buffer-file-name)))
                         (format "%s %s" (car mod) (cdr mod)))))
                 ((when-let* ((mod (cdr (assq major-mode doom--module-mode-alist))))
                    (format "%s %s"
                            (symbol-name (car mod))
                            (symbol-name (cadr mod)))))))
          (module-string
           (completing-read
            "Describe module: "
            (cl-loop for path in (doom-module-load-path 'all)
                     for (cat . mod) = (doom-module-from-path path)
                     for format = (format "%s %s" cat mod)
                     if (doom-module-p cat mod)
                     collect format
                     else
                     collect (propertize format 'face 'font-lock-comment-face))
            nil t nil nil module))
          (key (split-string module-string " ")))
     (list (intern (car  key))
           (intern (cadr key)))))
  (cl-check-type category symbol)
  (cl-check-type module symbol)
  (or (doom-module-p category module)
      (error "'%s %s' isn't a valid module" category module))
  (doom-project-browse (doom-module-path category module)))

(defun doom--describe-package-insert-button (label path &optional regexp)
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

;;;###autoload
(global-set-key [remap describe-package] #'doom/describe-package)

(defvar doom--describe-package-list-cache nil)
;;;###autoload
(defun doom/describe-package (package)
  "Like `describe-packages', but is Doom aware.

Only shows installed packages. Includes information about where packages are
defined and configured.

If prefix arg is prsent, refresh the cache."
  (interactive
   (list
    (let* ((guess (or (function-called-at-point)
                      (symbol-at-point))))
      (require 'finder-inf nil t)
      (require 'core-packages)
      (doom-initialize-packages)
      (let ((packages
             (or (unless current-prefix-arg doom--describe-package-list-cache)
                 (cl-loop for pkg
                          in (cl-delete-duplicates
                              (sort (append (mapcar #'car package-alist)
                                            (mapcar #'car package-archive-contents)
                                            (mapcar #'car package--builtins))
                                    #'string-greaterp))
                          if (assq pkg package-alist)
                          collect (symbol-name pkg)
                          else
                          collect (propertize (symbol-name pkg) 'face 'font-lock-comment-face)))))
        (unless (memq guess packages)
          (setq guess nil))
        (setq doom--describe-package-list-cache packages)
        (intern
         (completing-read
          (if guess
              (format "Describe package (default %s): "
                      guess)
            "Describe package: ")
          packages nil t nil nil
          (if guess (symbol-name guess))))))))
  (describe-package package)
  (save-excursion
    (with-current-buffer (help-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (when (and (doom-package-installed-p package)
                   (re-search-forward "^ *Status: " nil t))
          (end-of-line)
          (let ((indent (make-string (length (match-string 0)) ? )))
            (insert "\n" indent "Installed by the following Doom modules:\n")
            (dolist (m (get package 'doom-module))
              (insert indent)
              (doom--describe-package-insert-button
                (format "  %s %s" (car m) (or (cdr m) ""))
                (pcase (car m)
                  (:core doom-core-dir)
                  (:private doom-private-dir)
                  (category (doom-module-path category (cdr m)))))
              (insert "\n"))

            (package--print-help-section "Source")
            (pcase (doom-package-backend package)
              (`elpa (insert "[M]ELPA"))
              (`quelpa (insert (format "QUELPA %s" (prin1-to-string (doom-package-prop package :recipe)))))
              (`emacs (insert "Built-in")))
            (insert "\n")

            (package--print-help-section "Configs")
            (dolist (file (get package 'doom-files))
              (doom--describe-package-insert-button
                (abbreviate-file-name file)
                file
                (format "\\((\\(:?after!\\|def-package!\\)[ \t\n]*%s\\|^[ \t]*;; `%s'$\\)"
                        package package))
              (insert "\n" indent))
            (delete-char -1)))))))

;;;###autoload
(defun doom/describe-symbol (symbol)
  "Show help for SYMBOL, a variable, function or macro."
  (interactive
   (list (helpful--read-symbol "Symbol: " #'helpful--bound-p)))
  (let* ((sym (intern-soft symbol))
         (bound (boundp sym))
         (fbound (fboundp sym)))
    (cond ((and sym bound (not fbound))
           (helpful-variable sym))
          ((and sym fbound (not bound))
           (helpful-callable sym))
          ((apropos (format "^%s\$" symbol)))
          ((apropos (format "%s" symbol))))))

;;;###autoload
(defalias 'doom/help 'doom/open-manual)

;;;###autoload
(defun doom/open-manual ()
  "TODO"
  (interactive)
  (user-error "This command isn't implemented yet")
  ;; (find-file (expand-file-name "index.org" doom-docs-dir))
  )

;;;###autoload
(defun doom/open-news ()
  "TODO"
  (interactive)
  (user-error "This command isn't implemented yet")
  ;; (find-file (expand-file-name (concat "news/" doom-version) doom-docs-dir))
  )
