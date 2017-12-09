;;; ui/doom/config.el -*- lexical-binding: t; -*-

;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes
  :config
  (unless doom-theme
    (setq doom-theme 'doom-one)
    (after! solaire-mode
      (add-hook 'doom-init-ui-hook #'solaire-mode-swap-bg t)))

  ;; Ensure `doom/reload-load-path' reloads common faces
  (defun +doom|reload-theme () (load "doom-themes-common.el" nil t))
  (add-hook 'doom-pre-reload-theme-hook #'+doom|reload-theme)

  ;; improve integration w/ org-mode
  (add-hook 'doom-init-ui-hook #'doom-themes-org-config)

  ;; more Atom-esque file icons for neotree
  (add-hook 'doom-init-ui-hook #'doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)

  ;; blink mode-line on errors
  ;; FIXME Breaks modeline
  ;; (add-hook 'doom-init-ui-hook #'doom-themes-visual-bell-config)

  (after! neotree
    (defun +doom|neotree-fix-popup ()
      "Ensure the fringe settings are maintained on popup restore."
      (neo-global--when-window
       (doom--neotree-no-fringes)))
    (add-hook 'doom-popup-mode-hook #'+doom|neotree-fix-popup)))


(def-package! solaire-mode
  :hook (after-change-major-mode . turn-on-solaire-mode)
  :hook (doom-popup-mode . turn-off-solaire-mode)
  :config
  (setq solaire-mode-real-buffer-fn #'doom-real-buffer-p)

  ;; Prevent color glitches when reloading either DOOM or the theme
  (add-hook! '(doom-init-ui-hook doom-reload-hook) #'solaire-mode-reset)

  (add-hook!
    (gist-mode twittering-mode mu4e-view-mode org-tree-slide-mode +regex-mode)
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
