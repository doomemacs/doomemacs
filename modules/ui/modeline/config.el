;;; ui/modeline/config.el -*- lexical-binding: t; -*-

;; TODO Add themes (default, minimal, spacemacs, etc)

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
  (add-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
  (add-hook 'doom-modeline-mode-hook #'column-number-mode)   ; cursor column in modeline

  (add-hook 'doom-big-font-mode-hook #'+modeline|resize-for-big-font)
  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)

  (add-hook '+doom-dashboard-mode-hook #'doom-modeline-set-project-modeline)

  ;; Don't eager-load project.el. Doom only uses projectile anyway, for now.
  (defun +modeline*project-root ()
    (or doom-modeline-project-root
        (setq doom-modeline-project-root
              (file-local-name
               (or (and (featurep 'projectile) (ignore-errors (projectile-project-root)))
                   default-directory)))))
  (advice-add #'doom-modeline-project-root :override #'+modeline*project-root)

  ;; Magit -- modeline only where it's useful
  (defun +modeline|hide-in-non-status-buffer ()
    (if (eq major-mode 'magit-status-mode)
        (doom-modeline-set-project-modeline)
      (hide-mode-line-mode)))
  (add-hook 'magit-mode-hook #'+modeline|hide-in-non-status-buffer)

  ;; Show indentation style in modeline. I'm not using
  ;; `doom-modeline-def-segment' to prevent eager macro expansion from loading
  ;; the package too soon.
  (defun +modeline-indent-segment ()
    "indent modeline segment"
    (propertize (format "%s%d"
                        (if indent-tabs-mode "⭾" "␣")
                        tab-width)
                'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                'mouse-face 'mode-line-highlight
                'help-echo
                (let ((subsegs
                       (list (format "Indentation style: %s (%d wide)"
                                     (if indent-tabs-mode "tabs" "spaces")
                                     tab-width)
                             (cond ((eq doom-inhibit-indent-detection 'editorconfig)
                                    (propertize "✓ Editorconfig applied" 'face 'success))
                                   (doom-inhibit-indent-detection
                                    (propertize "✘ Indentation auto-detection disabled" 'face 'warning))
                                   ((bound-and-true-p dtrt-indent-original-indent)
                                    (propertize (format "✓ Indentation auto-detected (original: %s)"
                                                        dtrt-indent-original-indent)
                                                'face 'success)))
                             (when (bound-and-true-p ws-butler-mode)
                               (propertize "✓ ws-butler active (whitespace cleanup on save)"
                                           'face 'success)))))
                  (string-join (delq nil subsegs) "   "))))
  (add-to-list 'doom-modeline-fn-alist '(indent . +modeline-indent-segment))

  ;; Remove unused segments & extra padding
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(misc-info persp-name irc mu4e github debug indent input-method buffer-encoding lsp major-mode process vcs checker))

  (doom-modeline-def-modeline 'special
    '(bar window-number matches buffer-info-simple buffer-position selection-info)
    '(misc-info persp-name debug input-method irc-buffers buffer-encoding lsp major-mode process checker))

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
  :when (featurep! :feature evil)
  :after-call (evil-ex-start-search evil-ex-start-word-search))
