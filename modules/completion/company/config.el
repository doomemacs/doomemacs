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
        '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend
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
  :init (advice-add #'company-statistics-mode :around #'doom*shut-up)
  :config (setq company-statistics-file (concat doom-cache-dir "company-stats-cache.el")))


(def-package! company-box
  :when (and EMACS26+ (featurep! +childframe))
  :hook (company-mode . company-box-mode)
  :init
  (defun +company|fix-frontends ()
    "Ensure `company-pseudo-tooltip-frontend' is *not* in `company-frontends'
when company-box is active, which are incompatible and cause duplicate popups."
    (make-variable-buffer-local 'company-frontends)
    (if company-box-mode
        (setq company-frontends (delq 'company-pseudo-tooltip-frontend company-frontends))
      (cl-pushnew 'company-pseudo-tooltip-frontend company-frontends :test #'eq)))
  (add-hook 'company-box-mode-hook #'+company|fix-frontends)
  :config
  (setq company-box-backends-colors nil
        company-box-icons-elisp
        (list (all-the-icons-material "functions" :face 'all-the-icons-purple)
              (all-the-icons-material "check_circle" :face 'all-the-icons-blue)
              (all-the-icons-material "stars" :face 'all-the-icons-yellow)
              (all-the-icons-material "format_paint" :face 'all-the-icons-pink))
        company-box-icons-unknown (all-the-icons-material "find_in_page" :face 'all-the-icons-silver)
        company-box-icons-yasnippet (all-the-icons-material "short_text" :face 'all-the-icons-green)))


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

