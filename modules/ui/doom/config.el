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
  :config
  (unless doom-theme
    (setq doom-theme 'doom-one))

  ;; Reload common faces when reloading doom-themes live
  (defun +doom*reload-common (&rest _) (load "doom-themes-common.el" nil t))
  (advice-add #'doom//reload-theme :before #'+doom*reload-common)

  ;; improve integration w/ org-mode
  (add-hook 'doom-load-theme-hook #'doom-themes-org-config)

  ;; more Atom-esque file icons for neotree
  (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2))


(def-package! solaire-mode
  :commands (solaire-mode turn-on-solaire-mode solaire-mode-swap-bg)
  :init
  (defun +doom|solaire-mode-swap-bg-maybe ()
    (when-let* ((rule (assq doom-theme +doom-solaire-themes)))
      (require 'solaire-mode)
      (if (cdr rule) (solaire-mode-swap-bg))))
  (add-hook 'doom-load-theme-hook #'+doom|solaire-mode-swap-bg-maybe t)
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (setq solaire-mode-real-buffer-fn #'doom-real-buffer-p)
  ;; fringe can become unstyled when deleting or focusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  ;; Prevent color glitches when reloading either DOOM or loading a new theme
  (add-hook! :append '(doom-load-theme-hook doom-reload-hook)
    #'solaire-mode-reset)
  ;; org-capture takes an org buffer and narrows it. The result is erroneously
  ;; considered an unreal buffer, so solaire-mode must be restored.
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode))


(after! hideshow
  (defface +doom-folded-face
    `((((background dark))
       (:inherit font-lock-comment-face :background ,(doom-color 'base0) :weight light))
      (((background light))
       (:inherit font-lock-comment-face :background ,(doom-color 'base3) :weight light)))
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


;; NOTE Adjust these bitmaps if you change `doom-fringe-size'
(after! flycheck
  ;; because git-gutter is in the left fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

;; subtle diff indicators in the fringe
(after! git-gutter-fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))
