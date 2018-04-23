;;; completion/company/config.el -*- lexical-binding: t; -*-

(def-setting! :company-backend (modes &rest backends)
  "Prepends BACKENDS to `company-backends' in major MODES.

MODES should be one major-mode symbol or a list of them."
  `(progn
     ,@(cl-loop for mode in (doom-enlist (doom-unquote modes))
                for def-name = (intern (format "doom--init-company-%s" mode))
                collect
                `(defun ,def-name ()
                   (when (and (or (eq major-mode ',mode)
                                  (bound-and-true-p ,mode))
                              ,(not (eq backends '(nil))))
                     (require 'company)
                     (make-variable-buffer-local 'company-backends)
                     (dolist (backend (list ,@(reverse backends)))
                       (cl-pushnew backend company-backends :test #'equal))))
                collect `(add-hook! ,mode #',def-name))))


;;
;; Packages
;;

(def-package! company
  :commands (company-mode global-company-mode company-complete
             company-complete-common company-manual-begin company-grab-line)
  :init
  (setq company-idle-delay nil
        company-tooltip-limit 14
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-echo-metadata-frontend)
        company-backends
        '(company-capf company-dabbrev company-ispell company-yasnippet)
        company-transformers '(company-sort-by-occurrence))
  (when (featurep! +auto)
    (require 'company)
    (setq company-idle-delay 0.2))
  :config
  (global-company-mode +1))


(def-package! company-statistics
  :hook (company-mode . company-statistics-mode)
  :config (setq company-statistics-file (concat doom-cache-dir "company-stats-cache.el")))


(def-package! company-quickhelp
  :unless (and EMACS26+ (featurep! +childframe))
  :hook (company-mode . company-quickhelp-mode)
  :config (setq company-quickhelp-delay nil))


(def-package! company-box
  :when (and EMACS26+ (featurep! +childframe))
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-icons-elisp
        (list (concat (all-the-icons-material "functions") " ")
              (concat (all-the-icons-material "check_circle") " ")
              (concat (all-the-icons-material "stars") " ")
              (concat (all-the-icons-material "format_paint") " "))
        company-box-icons-unknown (concat (all-the-icons-material "find_in_page") " ")
        company-box-icons-yasnippet (concat (all-the-icons-material "short_text") " ")))


(def-package! company-dict
  :commands company-dict
  :config
  (defun +company|enable-project-dicts (mode &rest _)
    "Enable per-project dictionaries."
    (if (symbol-value mode)
        (cl-pushnew mode company-dict-minor-mode-list :test #'eq)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'doom-project-hook #'+company|enable-project-dicts))


;;
;; Included with company.el
;;

(autoload 'company-capf "company-capf")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-elisp "company-elisp")
(autoload 'company-etags "company-etags")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")
(autoload 'company-yasnippet "company-yasnippet")

