;;; ui/doom/config.el -*- lexical-binding: t; -*-

(defvar +doom-solaire-themes
  '((doom-city-lights . t)
    (doom-dracula . t)
    (doom-molokai . t)
    (doom-nord . t)
    (doom-nord-light . t)
    (doom-nova . nil)
    (doom-one . t)
    (doom-one-light . t)
    (doom-opera . t)
    (doom-solarized-light . nil)
    (doom-spacegrey . nil)
    (doom-vibrant . nil))
  "An alist of themes that support `solaire-mode'. If CDR is t, then use
`solaire-mode-swap-bg'.")


;;
;; Plugins
;;

;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes
  :defer t
  :init
  (unless doom-theme
    (setq doom-theme 'doom-one))
  :config
  ;; improve integration w/ org-mode
  (add-hook 'doom-load-theme-hook #'doom-themes-org-config)

  ;; more Atom-esque file icons for neotree/treemacs
  (when (featurep! :ui neotree)
    (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
    (setq doom-neotree-enable-variable-pitch t
          doom-neotree-file-icons 'simple
          doom-neotree-line-spacing 2))
  (when (featurep! :ui treemacs)
    (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config)
    (setq doom-treemacs-enable-variable-pitch t)))


(def-package! solaire-mode
  :defer t
  :init
  (defun +doom|solaire-mode-swap-bg-maybe ()
    (when-let* ((rule (assq doom-theme +doom-solaire-themes)))
      (require 'solaire-mode)
      (if (cdr rule) (solaire-mode-swap-bg))))
  (add-hook 'doom-load-theme-hook #'+doom|solaire-mode-swap-bg-maybe t)
  :config
  (add-hook 'change-major-mode-after-body-hook #'turn-on-solaire-mode)
  ;; fringe can become unstyled when deleting or focusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  ;; Prevent color glitches when reloading either DOOM or loading a new theme
  (add-hook! :append '(doom-load-theme-hook doom-reload-hook)
    #'solaire-mode-reset)
  ;; org-capture takes an org buffer and narrows it. The result is erroneously
  ;; considered an unreal buffer, so solaire-mode must be restored.
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode))


(after! hideshow
  (defface +doom-folded-face `((t (:inherit font-lock-comment-face :weight light)))
    "Face to hightlight `hideshow' overlays."
    :group 'doom-themes)

  ;; Nicer code-folding overlays (with fringe indicators)
  (defun +doom-set-up-overlay (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (when (featurep 'vimish-fold)
        (overlay-put
         ov 'before-string
         (propertize "…" 'display
                     (list vimish-fold-indication-mode
                           'empty-line
                           'vimish-fold-fringe))))
      (overlay-put
       ov 'display (propertize "  [...]  " 'face '+doom-folded-face))))
  (setq hs-set-up-overlay #'+doom-set-up-overlay))

