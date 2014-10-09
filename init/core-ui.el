(provide 'core-ui)

;; (global-linum-mode t)   ; line numbers for everybody!
(blink-cursor-mode -1)
(global-hl-line-mode 1)

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Multiple cursors across buffers cause a strange redraw delay for
;; some things, like auto-complete or evil-mode's cursor color
;; switching.
(setq-default cursor-in-non-selected-windows nil)

;; Show line/col-no in mode-line
(line-number-mode t)
(column-number-mode t)
;; make the fringe unintrusive
(when (fboundp 'fringe-mode)
  (fringe-mode '(0 . 10)))

;; Show full path in window title
(setq frame-title-format
      '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))

;; Whitespaces (activate whitespace-mode to see)
(setq whitespace-style '(face trailing tab-mark))

;; do not soft-wrap lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(add-hook! 'help-mode-hook (setq truncate-lines nil))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

(setq-default visible-bell nil)
(setq-default use-dialog-box nil)

;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)
(add-to-list 'default-frame-alist `(font . ,*font))
(add-to-list 'default-frame-alist '(alpha 98 95))   ; *slightly* transparent window

(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))

;;;; Modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-mode-line
  :config
  (mapc (lambda(mode) (add-to-list 'rm-excluded-modes mode))
        '(" SP"
          " Fill"
          " yas"
          " Fly"
          " EvilOrg"
          " Abbrev"
          ))
  :init
  (progn
    (setq sml/no-confirm-load-theme t
          sml/mode-width      'full
          sml/show-remote     nil
          sml/encoding-format nil)
    (add-to-list 'sml/replacer-regexp-list '("^:DB:projects/" "PR:") t)

    (sml/setup)
    (sml/apply-theme 'respectful)))
