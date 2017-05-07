;;; ui/doom/config.el

(defvar +doom-theme 'doom-one
  "The color theme currently in use.")

(defvar +doom-font
  (font-spec :family "Fira Mono" :size 12)
  "The font currently in use.")

(defvar +doom-variable-pitch-font
  (font-spec :family "Fira Sans" :size 14)
  "The font currently in use.")

(defvar +doom-unicode-font
  (font-spec :family "DejaVu Sans Mono" :size 12)
  "Fallback font for unicode glyphs.")


;;; Set fonts
(when (display-graphic-p)
  (with-demoted-errors "FONT ERROR: %s"
    (set-frame-font +doom-font t t)
    ;; Fallback to `doom-unicode-font' for Unicode characters
    (when +doom-unicode-font
      (set-fontset-font t 'unicode +doom-unicode-font))
    ;; ...and for variable-pitch mode
    (when +doom-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :font +doom-variable-pitch-font))))


;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 0
      window-divider-default-right-width 1)
(window-divider-mode +1)


;; doom-one: gives Emacs a look inspired by Dark One in Atom.
;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes :demand t
  :config
  (load-theme +doom-theme t)

  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (face-background 'doom-hl-line))

  (defface +doom-folded-face
    `((((background dark))
       (:inherit font-lock-comment-face :background ,(doom-color 'black)))
      (((background light))
       (:inherit font-lock-comment-face :background ,(doom-color 'light-grey))))
    "Face to hightlight `hideshow' overlays."
    :group 'doom)

  ;; Dark frames by default
  (when (display-graphic-p)
    (push (cons 'background-color (face-background 'default)) default-frame-alist)
    (push (cons 'foreground-color (face-foreground 'default)) default-frame-alist))

  (defun +doom|buffer-mode-on ()
    "Enable `doom-buffer-mode' in buffers that are real (see
`doom-real-buffer-p')."
    (when (and (not doom-buffer-mode)
               (doom-real-buffer-p))
      (doom-buffer-mode +1)))
  (add-hook 'after-change-major-mode-hook #'+doom|buffer-mode-on)

  (defun +doom|buffer-mode-off ()
    "Disable `doom-buffer-mode' in popup buffers."
    (when (and doom-buffer-mode
               (not (get-buffer-window-list)))
      (doom-buffer-mode -1)))
  (add-hook 'doom-popup-mode-hook #'+doom|buffer-mode-off)

  (when (featurep! :feature workspaces)
    (defun +doom|restore-bright-buffers (&rest _)
      "Restore `doom-buffer-mode' in buffers when `persp-mode' loads a session."
      (dolist (buf (persp-buffer-list))
        (with-current-buffer buf
          (+doom|buffer-mode-on))))
    (add-hook '+workspaces-load-session-hook #'+doom|restore-bright-buffers))

  ;; Add file icons to doom-neotree
  (doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 3)

  ;; Add line-highlighting to nlinum
  (doom-themes-nlinum-config))


;; Flashes the line around the cursor after any motion command that might
;; reasonably send the cursor somewhere the eyes can't follow. Tremendously
;; helpful on a 30" 2560x1600 display.
(def-package! nav-flash
  :commands nav-flash-show
  :init
  (defun doom/blink-cursor (&rest _)
    "Blink line, to keep track of the cursor."
    (interactive)
    (nav-flash-show))

  (add-hook! :append
    '(imenu-after-jump-hook evil-jumps-post-jump-hook find-file-hook)
    'doom/blink-cursor)

  (after! evil
    (advice-add #'evil-window-bottom :after #'doom/blink-cursor)
    (advice-add #'evil-window-middle :after #'doom/blink-cursor)
    (advice-add #'evil-window-top    :after #'doom/blink-cursor)))


(after! hideshow
  ;; Nicer code-folding overlays
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put
             ov 'display (propertize "  [...]  " 'face '+doom-folded-face))))))


;; subtle diff indicators in the fringe
(after! git-gutter-fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))
