;; Package management bootstrap
(setq package-enable-at-startup nil
      ;; package-archives
      ;; '(("melpa" . "http://melpa.milkbox.net/packages/")
      ;;   ("org" . "http://orgmode.org/elpa/")
      ;;   ("marmalade" . "http://marmalade-repo.org/packages/")
      ;;   ("gnu" . "http://elpa.gnu.org/packages/"))
      package-archive-exclude-alist
      '(("melpa" org-trello)
        ("melpa" org)
        ("marmalade" org)
        ("gnu" org)))

(let ((default-directory my/elisp-dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

;; (package-initialize)
(require 'use-package)
(require 'diminish)

;;
(provide 'core-packages)
