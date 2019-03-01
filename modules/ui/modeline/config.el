;;; ui/modeline/config.el -*- lexical-binding: t; -*-

;; TODO Add themes (default, minimal, spacemacs, etc)

(def-package! doom-modeline
  :hook (doom-post-init . doom-modeline-mode)
  :preface
  ;; prevent flash of unstyled modeline at startup
  (setq-default mode-line-format nil)
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  :init
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-checker-simple-format nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project)

  (add-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (add-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline

  :config
  (add-hook 'doom-big-font-mode-hook #'+modeline|resize-for-big-font)

  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)
  (add-hook '+doom-dashboard-mode-hook #'doom-modeline-set-project-modeline)

  ;; Remove unused segments & extra padding
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position selection-info)
    '(misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar matches buffer-info-simple buffer-position selection-info)
    '(misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar buffer-default-directory)
    '(misc-info mu4e github debug fancy-battery " " major-mode)))


;;
;; Extensions

(def-package! anzu
  :after-call isearch-mode)

(def-package! evil-anzu
  :when (featurep! :feature evil)
  :after-call (evil-ex-start-search evil-ex-start-word-search))
