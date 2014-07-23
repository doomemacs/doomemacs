(require-packages
 '(
   ;; uniquify                ; unique buffer names for identical filenames
   diminish                ; shrinks/removes modeline elements
   ))

(setq inhibit-startup-screen t)     ; don't show EMACs start screen
(fset 'yes-or-no-p 'y-or-n-p)       ; y/n instead of yes/no
(global-linum-mode t)               ; line numbers for everybody!

;; Sane scroll settings
(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Show line/col-no in mode-line
(line-number-mode t)
(column-number-mode t)

;; make the fringe unintrusive
(if (fboundp 'fringe-mode) (fringe-mode 8))

;; Line numbers with +1 left-padding
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string (+ w 1)) "d" (if window-system "" " ")))) ad-do-it))

;; Show full path in window title
(setq frame-title-format
      '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))

;;;#uniquify
;; (setq uniquify-buffer-name-style 'forward)
;; (setq uniquify-separator "/")
;; (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
;; (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Modeline settings

;; TODO: Customize modeline!


;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist `(font . ,my-font))
(add-to-list 'default-frame-alist '(alpha 98 95))   ; *slightly* transparent window

;; Use system clipboard
(setq ring-bell-function 'ignore)

(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))


;;
(provide 'core-ui)
