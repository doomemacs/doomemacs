;;; lang/coq/autoload.el -*- lexical-binding: t; -*-

;; HACK `proof-general' ascertains its own library path at compile time in its
;; autoloads file using `byte-compile-current-file' (and stores it in
;; `pg-init--script-full-path'). This means that when
;; `doom-package-autoload-file' is created and byte-compiled,
;; `pg-init--script-full-path' will be wrong, causing file-missing errors as it
;; tries to load `proof-site'. We prevent this by defining these two variables
;; early, in our own autoloads file.
;;;###autoload
(setq pg-init--script-full-path (locate-library "proof-general")
      pg-init--pg-root (file-name-directory pg-init--script-full-path))

;;;###autoload
(add-hook 'coq-mode-hook #'company-coq-mode)
