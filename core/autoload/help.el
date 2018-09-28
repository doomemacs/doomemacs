;;; core/autoload/help.el -*- lexical-binding: t; -*-

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
    (enh-ruby-mode :lang ruby)
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
(define-obsolete-function-alias 'doom/describe-setting 'doom/describe-setters "2.1.0")

;;;###autoload
(defun doom/describe-setters (setting)
  "Open the documentation of Doom functions and configuration macros."
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
          (setting
           (completing-read
            "Describe setter: "
            ;; TODO Could be cleaner (refactor me!)
            (cl-loop with maxwidth = (apply #'max (mapcar #'length (mapcar #'symbol-name settings)))
                     for def in (sort settings #'string-lessp)
                     if (or (get def 'doom-module)
                            (doom-module-from-path (symbol-file def)))
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "%s %s" (car it) (cdr it))
                                             'face 'font-lock-comment-face))
                     else if (file-in-directory-p (symbol-file def) doom-core-dir)
                     collect
                     (format (format "%%-%ds%%s" (+ maxwidth 4))
                             def (propertize (format "%s %s" :core (file-name-sans-extension (file-relative-name (symbol-file def) doom-core-dir)))
                                             'face 'font-lock-comment-face))
                     else
                     collect (symbol-name def))
            nil t
            (when (and (symbolp sym)
                       (string-match-p "!$" (symbol-name sym)))
              (symbol-name sym)))))
     (list (and setting (car (split-string setting " "))))))
  (or (stringp setting)
      (functionp setting)
      (signal 'wrong-type-argument (list '(stringp functionp) setting)))
  (let ((fn (if (functionp setting)
                setting
              (intern-soft setting))))
    (or (fboundp fn)
        (error "'%s' is not a valid DOOM setting" setting))
    (if (fboundp 'helpful-callable)
        (helpful-callable fn)
      (describe-function fn))))

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
  (let ((doc-path (doom-module-path category module "README.org")))
    (unless (file-exists-p doc-path)
      (error "There is no documentation for this module (%s)" doc-path))
    (find-file doc-path)))

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
(defun doom/what-face (arg &optional pos)
  "Shows all faces and overlay faces at point.

Interactively prints the list to the echo area. Noninteractively, returns a list
whose car is the list of faces and cadr is the list of overlay faces."
  (interactive "P")
  (let* ((pos (or pos (point)))
         (faces (let ((face (get-text-property pos 'face)))
                  (if (keywordp (car-safe face))
                      (list face)
                    (cl-loop for f in (doom-enlist face) collect f))))
         (overlays (cl-loop for ov in (overlays-at pos (1+ pos))
                            nconc (doom-enlist (overlay-get ov 'face)))))
    (cond ((called-interactively-p 'any)
           (message "%s %s\n%s %s"
                    (propertize "Faces:" 'face 'font-lock-comment-face)
                    (if faces
                        (cl-loop for face in faces
                                 if (or (listp face) arg)
                                   concat (format "'%s " face)
                                 else
                                   concat (concat (propertize (symbol-name face) 'face face) " "))
                      "n/a ")
                    (propertize "Overlays:" 'face 'font-lock-comment-face)
                    (if overlays
                        (cl-loop for ov in overlays
                                 if arg concat (concat (symbol-name ov) " ")
                                 else concat (concat (propertize (symbol-name ov) 'face ov) " "))
                      "n/a")))
          (t
           (and (or faces overlays)
                (list faces overlays))))))

;;;###autoload
(defalias 'doom/help 'doom/open-manual)

;;;###autoload
(defun doom/open-manual ()
  "TODO"
  (interactive)
  (find-file (expand-file-name "index.org" doom-docs-dir)))
