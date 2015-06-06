(defconst BASE-DIR      user-emacs-directory)

(defconst CORE-DIR      (eval-when-compile (concat BASE-DIR "core/")))
(defconst MODULES-DIR   (eval-when-compile (concat BASE-DIR "init/")))
(defconst CONTRIB-DIR   (eval-when-compile (concat BASE-DIR "contrib/")))
(defconst THEMES-DIR    (eval-when-compile (concat BASE-DIR "themes/")))
(defconst SNIPPETS-DIR  (eval-when-compile (concat BASE-DIR "snippets/")))
(defconst ELPA-DIR      (eval-when-compile (concat BASE-DIR ".cask/" emacs-version "/elpa/")))
(defconst TMP-DIR       (eval-when-compile (concat BASE-DIR ".cache-" (system-name) "/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (defmacro narf/init-load-path ()
    "Collect and verify `load-path'. Compile me!"
    (let (paths '())
      (dolist (dir (append (directory-files ELPA-DIR t "^[^.]" t)
                           (directory-files CONTRIB-DIR t "^[^.]" t)))
        (when (file-directory-p dir)
          (push dir paths)))
      `(setq load-path ',(append (list CORE-DIR CONTRIB-DIR MODULES-DIR)
                                 (if (listp load-path) load-path (list load-path))
                                 paths))))

  ;; Are you pondering what I'm pondering?
  (defmacro narf/init (packages)
    `(progn ,@(mapcar (lambda (pkg) `(require ',pkg)) (eval packages)))))

(narf/init-load-path)
;; (require 'benchmark)
(require 'autoloads nil t) ; use `make autoloads` to generate autoloads file

(setq custom-theme-directory THEMES-DIR)
(setq use-package-verbose DEBUG-MODE)
;;(setq use-package-expand-minimally (not DEBUG-MODE))
(eval-when-compile (require 'use-package))
(require 'diminish)

;;; startup.el ends here
