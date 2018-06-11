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

(defvar doom-docs-dir (concat doom-emacs-dir "docs/")
  "TODO")


;;
;; Helpers
;;

;;;###autoload
(defun doom-active-minor-modes ()
  "Return a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           if (and (boundp mode) (symbol-value mode))
           collect mode))



;;
;; Commands
;;

;;;###autoload
(defun doom/describe-setting (setting)
  "Open the documentation of SETTING (a keyword defined with `def-setting!').

Defaults to the "
  (interactive
   (let ((settings (cl-loop with case-fold-search = nil
                            for sym being the symbols of obarray
                            for sym-name = (symbol-name sym)
                            if (string-match "^doom--set\\(:.+\\)" sym-name)
                            collect (match-string 1 sym-name)))
         (sym (symbol-at-point)))
     (list (completing-read "Describe setting: "
                            (sort settings #'string-lessp)
                            nil t (if (keywordp sym) (symbol-name sym))))))
  (or (stringp setting)
      (signal 'wrong-type-argument (list 'stringp setting)))
  (let ((fn (intern-soft (format "doom--set%s" setting))))
    (or (fboundp fn)
        (error "'%s' is not a valid DOOM setting" setting))
    (describe-function fn)))

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
                           (symbol-name (cadr mod))))))))
     (list (completing-read "Describe module: "
                            (cl-loop for (module . sub) in (reverse (hash-table-keys doom-modules))
                                     collect (format "%s %s" module sub))
                            nil t nil nil module))))
  (cl-destructuring-bind (category submodule)
      (mapcar #'intern (split-string module " "))
    (unless (doom-module-p category submodule)
      (error "'%s' isn't a valid module" module))
    (let ((doc-path (doom-module-path category submodule "README.org")))
      (unless (file-exists-p doc-path)
        (error "There is no documentation for this module"))
      (find-file doc-path))))

;;;###autoload
(defun doom/describe-active-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Minor mode: "
                          (doom-active-minor-modes))))
  (describe-minor-mode-from-symbol
   (cl-typecase mode
     (string (intern mode))
     (symbol mode)
     (t (error "Expected a symbol/string, got a %s" (type-of mode))))))

;;;###autoload
(defun doom/what-face (&optional pos)
  "Shows all faces and overlay faces at point.

Interactively prints the list to the echo area. Noninteractively, returns a list
whose car is the list of faces and cadr is the list of overlay faces."
  (interactive)
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
                                 if (listp face)
                                   concat (format "'%s " face)
                                 else
                                   concat (concat (propertize (symbol-name face) 'face face) " "))
                      "n/a ")
                    (propertize "Overlays:" 'face 'font-lock-comment-face)
                    (if overlays
                        (cl-loop for ov in overlays
                                 concat (concat (propertize (symbol-name ov) 'face ov) " "))
                      "n/a")))
          (t
           (and (or faces overlays)
                (list faces overlays))))))

;;;###autoload
(defun doom//open-manual ()
  (interactive)
  (find-file (expand-file-name "index.org" doom-docs-dir)))

;;;###autoload
(defun doom//reload (&optional force-p)
  "Reloads your config. This is experimental!

If called from a noninteractive session, this will try to communicate with a
live server (if one is found) to tell it to run this function.

If called from an interactive session, tries to reload autoloads files (if
necessary), reinistalize doom (via `doom-initialize') and reloads your private
init.el and config.el. Then runs `doom-reload-hook'."
  (interactive)
  (cond ((and noninteractive (not (daemonp)))
         (require 'server)
         (if (not (server-running-p))
             (doom//reload-autoloads force-p)
           (print! "Reloading active Emacs session...")
           (print!
            (bold "%%s")
            (if (server-eval-at server-name '(doom//reload))
                (green "Done!")
              (red "There were issues!")))))
        ((let ((load-prefer-newer t))
           (doom//reload-autoloads force-p)
           (doom-initialize 'force)
           (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
             (doom-initialize-modules 'force))
           (print! (green "%d packages reloaded" (length package-alist)))
           (run-hooks 'doom-reload-hook)
           t))))
