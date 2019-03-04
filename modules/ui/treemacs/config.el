;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(setq treemacs-follow-after-init t
      treemacs-is-never-other-window t
      treemacs-sorting 'alphabetic-case-insensitive-desc
      treemacs-persist-file (concat doom-cache-dir "treemacs-persist"))

(after! treemacs-persistence
  ;; This variable is defined with defconst, so we must wait to change it until
  ;; it has loaded.
  (setq treemacs--last-error-persist-file
        (concat doom-cache-dir
                "treemacs-persist-at-last-error")))


(after! treemacs
  (set-popup-rule! "^ \\*Treemacs"
    :side treemacs-position
    :size treemacs-width
    :quit nil
    :ttl 0)

  (defun +treemacs|improve-hl-line-contrast ()
    "`hl-line' doesn't stand out enough in some themes."
    (face-remap-add-relative 'hl-line 'region))
  (add-hook 'treemacs-mode-hook #'+treemacs|improve-hl-line-contrast)

  ;; Don't follow the cursor
  (treemacs-follow-mode -1)

  (after! ace-window
    (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))))


(def-package! treemacs-evil
  :when (featurep! :feature evil +everywhere)
  :after treemacs)


(def-package! treemacs-projectile
  :after treemacs)
