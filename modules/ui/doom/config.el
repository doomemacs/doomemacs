;;; ui/doom/config.el

(defvar +doom-theme 'doom-one
  "The color theme to use.")

(defvar +doom-font
  (font-spec :family "Fira Mono" :size 12)
  "The font currently in use.")

(defvar +doom-variable-pitch-font
  (font-spec :family "Fira Sans" :size 13)
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
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode +1)


;; doom-one: gives Emacs a look inspired by Dark One in Atom.
;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes :demand t
  :config
  (load-theme +doom-theme t)

  ;; Add file icons to doom-neotree
  (doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)

  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (face-background 'doom-hl-line))

  ;; Dark frames by default
  (when (display-graphic-p)
    (push (cons 'background-color (face-background 'default)) initial-frame-alist)
    (push (cons 'foreground-color (face-foreground 'default)) initial-frame-alist))

  (defun +doom|buffer-mode-on ()
    "Enable `doom-buffer-mode' in buffers that are real (see
`doom-real-buffer-p')."
    (when (and (not doom-buffer-mode)
               (doom-real-buffer-p))
      (doom-buffer-mode +1)))
  (add-hook 'after-change-major-mode-hook #'+doom|buffer-mode-on)

  (defun +doom|buffer-mode-off ()
    "Disable `doom-buffer-mode' in popup buffers."
    (when doom-buffer-mode
      (doom-buffer-mode -1)))
  (add-hook 'doom-popup-mode-hook #'+doom|buffer-mode-off)

  ;;
  (add-hook '+workspaces-load-session-hook #'+doom|restore-bright-buffers)

  ;; Extra modes to activate doom-buffer-mode in
  (add-hook! (gist-mode
              twittering-mode
              mu4e-view-mode
              org-tree-slide-mode
              +regex-mode)
    #'doom-buffer-mode)

  (after! neotree
    (defun +doom|neotree-fix-popup ()
      "Ensure the fringe settings are maintained on popup restore."
      (neo-global--when-window
        (doom--neotree-no-fringes)))
    (add-hook 'doom-popup-mode-hook #'+doom|neotree-fix-popup nil t)))


;; Flashes the line around the cursor after any motion command that might
;; reasonably send the cursor somewhere the eyes can't follow. Tremendously
;; helpful on a 30" 2560x1600 display.
(def-package! nav-flash
  :commands nav-flash-show
  :init
  (defun doom*blink-cursor-maybe (orig-fn &rest args)
    "Blink current line if the window has moved."
    (interactive)
    (let ((point (save-excursion (goto-char (window-start))
                                 (point-marker))))
      (apply orig-fn args)
      (unless (equal point
                     (save-excursion (goto-char (window-start))
                                     (point-marker)))
        (doom/blink-cursor))))

  (defun doom/blink-cursor (&rest _)
    "Blink current line using `nav-flash'."
    (interactive)
    (unless (minibufferp)
      (nav-flash-show)
      ;; only show in the current window
      (overlay-put compilation-highlight-overlay 'window (selected-window))))

  (add-hook!
    '(imenu-after-jump-hook evil-jumps-post-jump-hook find-file-hook)
    #'doom/blink-cursor)

  (advice-add #'windmove-do-window-select :around #'doom*blink-cursor-maybe)
  (advice-add #'recenter :around #'doom*blink-cursor-maybe)

  (after! evil
    (advice-add #'evil-window-top    :after #'doom/blink-cursor)
    (advice-add #'evil-window-middle :after #'doom/blink-cursor)
    (advice-add #'evil-window-bottom :after #'doom/blink-cursor)))


(after! hideshow
  (defface +doom-folded-face
    `((((background dark))
       (:inherit font-lock-comment-face :background ,(doom-color 'black)))
      (((background light))
       (:inherit font-lock-comment-face :background ,(doom-color 'light-grey))))
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


(when (display-graphic-p)
  (after! flycheck
    ;; because git-gutter is in the left fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0]))

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
