;;; ui/modeline/config.el -*- lexical-binding: t; -*-

(def-package! doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project)

  ;; Fix modeline icons in daemon-spawned graphical frames. We have our own
  ;; mechanism for disabling all-the-icons, so we don't need doom-modeline to do
  ;; it for us. However, this may cause unwanted padding in the modeline in
  ;; daemon-spawned terminal frames. If it bothers you, you may prefer
  ;; `doom-modeline-icon' set to `nil'.
  (when (daemonp)
    (setq doom-modeline-icon t))
  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (add-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline

  (add-hook 'doom-change-font-size-hook #'+modeline|resize-for-font)
  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)

  (add-hook '+doom-dashboard-mode-hook #'doom-modeline-set-project-modeline)

  (defun +modeline*project-root ()
    "Only use projectile-project-root."
    (or doom-modeline-project-root
        (setq doom-modeline-project-root
              (file-local-name
               (or (and (featurep 'projectile)
                        (ignore-errors (projectile-project-root)))
                   default-directory)))))
  (advice-add #'doom-modeline-project-root :override #'+modeline*project-root)

  (defun +modeline|hide-in-non-status-buffer ()
    "Show minimal modeline in magit-status buffer, no modeline elsewhere."
    (if (eq major-mode 'magit-status-mode)
        (doom-modeline-set-project-modeline)
      (hide-mode-line-mode)))
  (add-hook 'magit-mode-hook #'+modeline|hide-in-non-status-buffer)

  ;; Remove unused segments & extra padding
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar window-number matches buffer-info-simple buffer-position selection-info)
    '(objed-state misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))

  (doom-modeline-def-modeline 'project
    '(bar window-number buffer-default-directory)
    '(misc-info mu4e github debug fancy-battery " " major-mode process))

  ;; Some functions modify the buffer, causing the modeline to show a false
  ;; modified state, so we try to force them to behave.
  (defun +modeline*inhibit-modification-hooks (orig-fn &rest args)
    (with-silent-modifications (apply orig-fn args)))
  (advice-add #'ws-butler-after-save :around #'+modeline*inhibit-modification-hooks))


;;
;; Extensions

(def-package! anzu
  :after-call isearch-mode)

(def-package! evil-anzu
  :when (featurep! :editor evil)
  :after-call (evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight))
