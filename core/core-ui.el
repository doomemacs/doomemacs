;; User interface layout & behavior
(provide 'core-ui)

;;;; Load Theme ;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  ;; No transparency!
  (set-frame-parameter nil 'alpha 96)

  (unless (member *default-font (font-family-list))
    (defconst *default-font *alt-font))

  (unless (member *default-font (font-family-list))
    (error "Font %s isn't installed" *default-font))

  (let ((font-str (concat *default-font "-" (number-to-string *default-font-size))))
    (add-to-list 'default-frame-alist `(font . ,font-str))
    (add-to-list 'initial-frame-alist `(font . ,font-str))))

(add-to-list 'custom-theme-load-path my-themes-dir)
(load-dark-theme)


;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;;
(tooltip-mode -1)
(blink-cursor-mode 1)        ; blink cursor
;; (global-hl-line-mode 1)   ; highlight line

(setq linum-format " %3d")

;; Multiple cursors across buffers cause a strange redraw delay for
;; some things, like auto-complete or evil-mode's cursor color
;; switching.
(setq-default cursor-in-non-selected-windows nil)

(setq-default visible-bell nil)   ; silence of the bells
(setq-default use-dialog-box nil) ; avoid GUI
(setq-default redisplay-dont-pause t)

;; do not soft-wrap lines
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))    ; no scrollbar
(when (functionp 'tool-bar-mode)   (tool-bar-mode -1))      ; no toolbar
(when (functionp 'menu-bar-mode)   (menu-bar-mode -1))      ; no menubar
(when (fboundp 'fringe-mode) (fringe-mode '(5 . 10)))       ; no nonsense

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode 1))

;; Show full path in window title
(setq frame-title-format
      '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))


;;;; Modeline ;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-ignore-buffers-re "^\\*"))

(use-package smart-mode-line
  :config
  (setq rm-blacklist
        (mapconcat 'identity
                   '(" SP"
                     " Fill"
                     " yas"
                     " Fly"
                     " EvilOrg"
                     " Abbrev"
                     " WS"
                     " GitGutter"
                     " Undo-Tree"
                     " Projectile\\[.+\\]"
                     " hs"
                     " ElDoc"
                     ) "\\|"))
  :init
  (progn
    (setq sml/no-confirm-load-theme t
          sml/mode-width      'full
          sml/extra-filler    (if window-system -1 0)
          sml/show-remote     nil
          sml/modified-char   "*"
          sml/encoding-format nil)

    (setq sml/replacer-regexp-list '(("^~/Dropbox/Projects/" "PROJECTS:")
                                     ("^~/.emacs.d/" "EMACS.D:")))

    (after "evil" (setq evil-mode-line-format nil))

    (setq-default mode-line-misc-info
      '((which-func-mode ("" which-func-format ""))
        (global-mode-string ("" global-mode-string ""))))

    (add-hook! 'after-change-major-mode-hook
               (setq mode-line-format
                     '((if window-system " ")
                       "%e"
                       mode-line-mule-info
                       mode-line-client
                       mode-line-remote
                       mode-line-frame-identification
                       mode-line-buffer-identification
                       mode-line-modified
                       mode-line-modes
                       mode-line-misc-info
                       (vc-mode vc-mode)
                       " "
                       mode-line-position
                       " "
                       mode-line-front-space
                       ))

               (add-to-list 'mode-line-modes
                            '(sml/buffer-identification-filling
                              sml/buffer-identification-filling
                              (:eval (setq sml/buffer-identification-filling
                                           (sml/fill-for-buffer-identification))))))

    (let ((-linepo mode-line-position))
      (sml/setup)
      (sml/apply-theme 'respectful)

      (setq mode-line-position -linepo)
      (sml/filter-mode-line-list 'mode-line-position))))
