(provide 'core-ui)

;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)
(add-to-list 'default-frame-alist `(font . ,*font))
(add-to-list 'default-frame-alist '(alpha . 100))   ; *slightly* transparent window

(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))    ; no scrollbar
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))        ; no toolbar
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))        ; no menubar
(when (fboundp 'fringe-mode) (fringe-mode '(5 . 10)))       ; no nonsense

(defun toggle-transparency ()
  (interactive)
  (let ((frame (selected-frame)))
    (if (= (frame-parameter frame 'alpha) 0)
        (set-frame-parameter frame 'alpha 100)
      (set-frame-parameter frame 'alpha 0))))


;;;; Other Settings ;;;;;;;;;;;;;;;;;;;;
(blink-cursor-mode 1)       ; blink cursor
(global-hl-line-mode -1)    ; highlight line
(line-number-mode 1)        ; hide line no in modeline
(column-number-mode 1)      ; hide col no in modeline
(show-paren-mode -1)         ; highlight matching delimiters
(setq show-paren-delay 0)

(setq linum-format (quote "%4d "))
;; (add-hook 'text-mode-hook 'linum-mode)
;; (add-hook 'prog-mode-hook 'linum-mode)

;; Show full path in window title
(setq frame-title-format
      '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))

;; Multiple cursors across buffers cause a strange redraw delay for
;; some things, like auto-complete or evil-mode's cursor color
;; switching.
(setq-default cursor-in-non-selected-windows nil)

;; do not soft-wrap lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(add-hook! 'help-mode-hook (setq truncate-lines nil))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)
(setq-default visible-bell nil)   ; silence of the bells
(setq-default use-dialog-box nil) ; avoid GUI


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
                     " Anzu"
                     " Undo-Tree"
                     " AC"
                     " Projectile\\[.+\\]"
                     ) "\\|"))
  :init
  (progn
    (setq sml/no-confirm-load-theme t
          sml/mode-width      'full
          sml/extra-filler    2
          sml/show-remote     nil
          sml/modified-char   "*"
          sml/encoding-format nil)

    (setq sml/replacer-regexp-list '(("^~/Dropbox/Projects/" "PROJECTS:")
                                     ("^~/.emacs.d/" "EMACS.D:")))

    (after evil (setq evil-mode-line-format nil))

    (setq-default mode-line-misc-info
      '((which-func-mode ("" which-func-format ""))
        (global-mode-string ("" global-mode-string ""))))

    (add-hook! 'after-change-major-mode-hook
               (setq mode-line-format
                     '(" "
                       "%e"
                       ;; anzu--mode-line-format
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
