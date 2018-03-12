;;; core/autoload/help.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/describe-setting (setting)
  "Open the documentation of SETTING (a keyword defined with `def-setting!').

Defaults to the "
  (interactive
   (let ((sym (symbol-at-point)))
     (list (completing-read "Describe setting: "
                            (sort (mapcar #'car doom-settings) #'string-lessp)
                            nil t (if (keywordp sym) (symbol-name sym))))))
  (let ((fn (cdr (assq (intern setting) doom-settings))))
    (unless fn
      (error "'%s' is not a valid DOOM setting" setting))
    (describe-function fn)))


;;
(defvar doom--module-mode-alist
  '((c-mode :lang cc)
    (c++-mode :lang cc)
    (objc++-mode :lang cc)
    (java-mode :lang java)
    (csharp-mode :lang csharp)
    (clojure-mode :lang clojure)
    (emacs-lisp-mode :lang emacs-lisp)
    (go-mode :lang go)
    (haskell-mode :lang haskell)
    (js2-mode :lang javascript)
    (julia-mode :lang julia)
    (latex-mode :lang latex)
    (LaTeX-mode :lang latex)
    (ledger-mode :lang ledger)
    (lua-mode :lang lua)
    (markdown-mode :lang markdown)
    (gfm-mode :lang markdown)
    (ocaml-mode :lang ocaml)
    (org-mode :lang org)
    (perl-mode :lang perl)
    (php-mode :lang php)
    (hack-mode :lang php)
    (plantuml-mode :lang plantuml)
    (purescript-mode :lang purescript)
    (python-mode :lang python)
    (restclient-mode :lang rest)
    (ruby-mode :lang ruby)
    (rust-mode :lang rust)
    (scala-mode :lang scala)
    (sh-mode :lang sh)
    (swift-mode :lang swift)
    (typescript-mode :lang typescript)
    (web-mode :lang web)
    (css-mode :lang web)
    (scss-mode :lang web)
    (sass-mode :lang web)
    (less-css-mode :lang web)
    (stylus-mode :lang web))
  "TODO")

;;;###autoload
(defun doom/describe-module (module)
  "Open the documentation of MODULE (a string that represents the category and
submodule in the format, e.g. ':feature evil').

Defaults to either a) the module at point (in init.el), b) the module derived
from a `featurep!' or `require!' call, c) the module that the current file is
in, or d) the module associated with the current major mode (see
`doom--module-mode-alist')."
  (interactive
   (let ((module
          (cond ((and buffer-file-name
                      (eq major-mode 'emacs-lisp-mode)
                      (string= (file-name-nondirectory buffer-file-name)
                               "init.el")
                      (thing-at-point 'sexp t)))
                ((save-excursion
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
                           (symbol-name (cadr mod))))))))
     (list (completing-read "Describe module: "
                            (cl-loop for (module . sub) in (reverse (hash-table-keys doom-modules))
                                     collect (format "%s %s" module sub))
                            nil t module))))
  (cl-destructuring-bind (category submodule)
      (mapcar #'intern (split-string module " "))
    (unless (doom-module-p category submodule)
      (error "'%s' isn't a valid module" module))
    (let ((doc-path (doom-module-expand-file category submodule "README.org")))
      (unless (file-exists-p doc-path)
        (error "There is no documentation for this module"))
      (find-file doc-path))))

;;;###autoload
(defun doom*fix-helpful-prettyprint (value)
  "TODO"
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode))
    (pp value (current-buffer))
    (unless (or (symbolp value) (booleanp value) (keymapp value))
      (unless (hash-table-p value)
        (fill-region (point-min) (point-max)))
      (quiet! (indent-region (point-min) (point-max))))
    (string-trim (buffer-string))))
