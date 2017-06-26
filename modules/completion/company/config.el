;;; completion/company/config.el -*- lexical-binding: t; -*-

(def-setting! :company-backend (modes backends)
  "Register company BACKENDS to MODES."
  (let ((backends (doom-enlist (doom-unquote backends))))
    `(progn
       ,@(cl-loop for mode in (doom-enlist (doom-unquote modes))
                  for def-name = (intern (format "doom--init-company-%s" mode))
                  collect `(defun ,def-name ()
                             (when (eq major-mode ',mode)
                               (require 'company)
                               ,@(cl-loop for backend in backends
                                          collect `(cl-pushnew ',backend company-backends :test #'equal))))
                  collect `(add-hook! ,mode #',def-name)))))


;;
;; Packages
;;

(def-package! company
  :commands (company-mode global-company-mode company-complete
             company-complete-common company-manual-begin company-grab-line)
  :config
  (setq company-idle-delay nil
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf))

  (push #'company-sort-by-occurrence company-transformers)

  (after! yasnippet
    (nconc company-backends '(company-yasnippet)))

  (global-company-mode +1))


(def-package! company-statistics
  :after company
  :config
  (setq company-statistics-file (concat doom-cache-dir "company-stats-cache.el"))
  (quiet! (company-statistics-mode +1)))


;; Looks ugly on OSX without emacs-mac build
(def-package! company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))


(def-package! company-dict
  :commands company-dict
  :config
  ;; Project-specific dictionaries
  (defun +company|enable-project-dicts (mode &rest _)
    (if (symbol-value mode)
        (push mode company-dict-minor-mode-list)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'doom-project-hook #'+company|enable-project-dicts))


;;
;; Autoloads
;;

(autoload 'company-capf "company-capf")
(autoload 'company-yasnippet "company-yasnippet")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-etags "company-etags")
(autoload 'company-elisp "company-elisp")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")

