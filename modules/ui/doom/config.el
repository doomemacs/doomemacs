;;; ui/doom/config.el

(defvar +doom-theme 'doom-one
  "The color theme to use.")

(defvar +doom-font
  (font-spec :family "Fira Mono" :size 12)
  "The font currently in use.")

(defvar +doom-variable-pitch-font
  (font-spec :family "Fira Sans" :size 12)
  "The font currently in use.")

(defvar +doom-unicode-font
  (font-spec :family "DejaVu Sans Mono" :size 12)
  "Fallback font for unicode glyphs. Is ignored if :feature unicode is active.")


;;; Set fonts
(when (display-graphic-p)
  (with-demoted-errors "FONT ERROR: %s"
    (set-frame-font +doom-font t t)
    ;; Fallback to `doom-unicode-font' for Unicode characters
    (unless (featurep! :ui unicode)
      (when +doom-unicode-font
        (set-fontset-font t 'unicode +doom-unicode-font)))
    ;; ...and for variable-pitch mode
    (when +doom-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :font +doom-variable-pitch-font))))


;; doom-one: gives Emacs a look inspired by Dark One in Atom.
;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes
  :load-path "~/work/plugins/emacs-doom-themes/"
  :demand t
  :config
  (load-theme +doom-theme t)

  ;; blink mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Add file icons to doom-neotree
  (doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)

  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line))

  ;; Dark frames by default
  (when (display-graphic-p)
    (push (cons 'background-color (doom-color 'bg)) initial-frame-alist)
    (push (cons 'foreground-color (doom-color 'fg)) initial-frame-alist))

  (after! neotree
    (defun +doom|neotree-fix-popup ()
      "Ensure the fringe settings are maintained on popup restore."
      (neo-global--when-window
        (doom--neotree-no-fringes)))
    (add-hook 'doom-popup-mode-hook #'+doom|neotree-fix-popup nil t)))


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


(when (and (display-graphic-p) (fboundp 'define-fringe-bitmap))
  ;; NOTE Adjust these bitmaps if you change `doom-ui-fringe-size'
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
      "XXXX....")))
