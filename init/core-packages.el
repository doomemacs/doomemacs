
;; (require 'cl)

;; Package management bootstrap
(setq package-user-dir (expand-file-name "vendor" my/dir))
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(let ((default-directory my/elisp-dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

(package-initialize)

;; Check if a package is installed; if load is t, load it too.
;; Works for packages bundled with emacs too!
(defun my/install-package (package)
  (unless skip-installs
    (message "=> checking: %s" package)
    (unless (package-installed-p package)
      (unless (assoc package package-archive-contents)
        (package-refresh-contents))
      (message "=> installing: %s" package)
      (package-install package))))

(require 'use-package)

;;
(provide 'core-packages)
