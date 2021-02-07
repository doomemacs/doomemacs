;;; app/everywhere/config.el -*- lexical-binding: t; -*-

(use-package! emacs-everywhere
  :defer ; entry point is autoloaded
  :config
  (after! doom-modeline
    (doom-modeline-def-segment emacs-everywhere
      (concat (doom-modeline-spc)
              (when (emacs-everywhere-markdown-p)
                (concat
                 (all-the-icons-octicon "markdown" :face 'all-the-icons-green :v-adjust 0.02)
                 (doom-modeline-spc)))
              (propertize emacs-everywhere-app-name
                          'face 'doom-modeline-project-dir)
              (doom-modeline-spc)
              (propertize (truncate-string-to-width emacs-everywhere-window-title
                                                    45 nil nil "…")
                          'face 'doom-modeline-buffer-minor-mode)))
    (doom-modeline-def-modeline 'emacs-everywhere
      '(bar modals emacs-everywhere buffer-position word-count parrot selection-info)
      '(input-method major-mode checker))
    (defun emacs-everywhere-set-modeline ()
      (doom-modeline-set-modeline 'emacs-everywhere))
    (add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-modeline))
  (when (featurep! :ui workspaces)
    (defun emacs-everywhere-clear-persp-info ()
      (setq persp-emacsclient-init-frame-behaviour-override nil))
    (add-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-clear-persp-info))
  (after! solaire-mode
    (add-hook 'emacs-everywhere-init-hooks #'solaire-mode)))
