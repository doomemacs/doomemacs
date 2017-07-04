;;; ui/doom/config.el -*- lexical-binding: t; -*-

(set! :font "Fira Mono" :size 12)
(set! :big-font "Fira Mono" :size 18)
(set! :variable-font "Fira Sans" :size 12)
(set! :unicode-font "DejaVu Sans Mono" :size 12)

;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes
  :demand t
  :config
  (set! :theme 'doom-one)

  ;; Ensure `doom/reload' reloads common faces
  (defun +doom|reload-theme ()
    (load "doom-themes-common.el" nil t))
  (add-hook 'doom-pre-reload-theme-hook #'+doom|reload-theme)

  ;; blink mode-line on errors
  (add-hook 'doom-post-init-hook #'doom-themes-visual-bell-config)

  ;; Add file icons to doom-neotree
  (add-hook 'doom-post-init-hook #'doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)

  (after! neotree
    (defun +doom|neotree-fix-popup ()
      "Ensure the fringe settings are maintained on popup restore."
      (neo-global--when-window
       (doom--neotree-no-fringes)))
    (add-hook 'doom-popup-mode-hook #'+doom|neotree-fix-popup)))


(def-package! solaire-mode
  :commands (solaire-mode turn-on-solaire-mode turn-off-solaire-mode)
  :init
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'doom-popup-mode-hook #'turn-off-solaire-mode)
  :config
  (setq solaire-mode-real-buffer-fn #'doom-real-buffer-p)

  ;; Prevent color glitches when reloading either DOOM or the theme
  (defun +doom|reset-solaire-mode (&rest _) (solaire-mode-reset))
  (advice-add #'load-theme :after #'+doom|reset-solaire-mode)
  (add-hook 'doom-reload-hook #'solaire-mode-reset)
  (add-hook 'doom-init-ui-hook #'solaire-mode-reset)

  ;; Extra modes to activate doom-buffer-mode in
  (add-hook! (gist-mode
              twittering-mode
              mu4e-view-mode
              org-tree-slide-mode
              +regex-mode)
    #'solaire-mode))


(after! hideshow
  (defface +doom-folded-face
    `((((background dark))
       (:inherit font-lock-comment-face :background ,(doom-color 'base0)))
      (((background light))
       (:inherit font-lock-comment-face :background ,(doom-color 'base3))))
    "Face to hightlight `hideshow' overlays."
    :group 'doom)

  ;; Nicer code-folding overlays (with fringe indicators)
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (when (featurep 'vimish-fold)
              (overlay-put
               ov 'before-string
               (propertize "…" 'display
                           (list vimish-fold-indication-mode
                                 'empty-line
                                 'vimish-fold-fringe))))
            (overlay-put
             ov 'display (propertize "  [...]  " 'face '+doom-folded-face))))))


;; NOTE Adjust these bitmaps if you change `doom-fringe-size'
(after! flycheck
  ;; because git-gutter is in the left fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...X...."
    "..XX...."
    ".XXX...."
    "XXXX...."
    ".XXX...."
    "..XX...."
    "...X...."))

;; subtle diff indicators in the fringe
(after! git-gutter-fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."))
