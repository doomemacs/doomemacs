;;; app/everywhere/config.el -*- lexical-binding: t; -*-

(use-package! emacs-everywhere
  ;; Entry points into this package are autoloaded; i.e. the `emacs-everywhere'
  ;; function, meant to be called directly via emacsclient. See this module's
  ;; readme for details.
  :defer t
  :config
  (set-yas-minor-mode! 'emacs-everywhere-mode)

  (after! doom-modeline
    (doom-modeline-def-segment emacs-everywhere
      (concat
       (doom-modeline-spc)
       (when (emacs-everywhere-markdown-p)
         (concat
          (all-the-icons-octicon "markdown" :face 'all-the-icons-green :v-adjust 0.02)
          (doom-modeline-spc)))
       (propertize (emacs-everywhere-app-class emacs-everywhere-current-app)
                   'face 'doom-modeline-project-dir)
       (doom-modeline-spc)
       (propertize (truncate-string-to-width
                    (emacs-everywhere-app-title emacs-everywhere-current-app)
                    45 nil nil "…")
                   'face 'doom-modeline-buffer-minor-mode)))
    (doom-modeline-def-modeline 'emacs-everywhere
      '(bar modals emacs-everywhere buffer-position word-count parrot selection-info)
      '(input-method major-mode checker))
    (add-hook! 'emacs-everywhere-mode-hook
      (defun +everywhere-set-modeline ()
        (doom-modeline-set-modeline 'emacs-everywhere))))
  (add-hook! 'emacs-everywhere-init-hooks
    (defun +everywhere-clear-persp-info-h ()
      (when (bound-and-true-p persp-mode)
        (setq persp-emacsclient-init-frame-behaviour-override nil)))))
