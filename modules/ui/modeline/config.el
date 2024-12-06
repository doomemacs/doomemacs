;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(when (modulep! +light)
  (load! "+light"))


(use-package! doom-modeline
  :unless (modulep! +light)
  :hook (doom-after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type (if (featurep :system 'windows) 1 0))

  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)

  (add-to-list 'doom-modeline-mode-alist '(+doom-dashboard-mode . dashboard))
  (add-hook! 'magit-mode-hook
    (defun +modeline-hide-in-non-status-buffer-h ()
      "Show minimal modeline in magit-status buffer, no modeline elsewhere."
      (if (eq major-mode 'magit-status-mode)
          (doom-modeline-set-modeline 'magit)
        (hide-mode-line-mode))))


  ;;
  ;;; Extensions
  (use-package! anzu
    :after-call isearch-mode)

  (use-package! evil-anzu
    :when (modulep! :editor evil)
    :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
    :config (global-anzu-mode +1)))
