(provide 'core-ui)

;; (global-linum-mode t)   ; line numbers for everybody!
;; (blink-cursor-mode -1)

;; Show line/col-no in mode-line
(line-number-mode t)
(column-number-mode t)

;; make the fringe unintrusive
(when (fboundp 'fringe-mode) (fringe-mode 8))

;; Line numbers with +1 left-padding
;; (defadvice linum-update-window (around linum-dynamic activate)
;;   (let* ((w (length (number-to-string (count-lines (point-min) (point-max)))))
;;          (linum-format (concat "%" (number-to-string (+ w 1)) "d" " "))) ad-do-it))

;; Show full path in window title
(setq frame-title-format
      '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))

;; Whitespaces (activate whitespace-mode to see)
(setq whitespace-style '(face trailing tab-mark))

;; do not soft-wrap lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq indicate-buffer-boundaries nil)
(setq indicate-empty-lines nil)

(setq-default visible-bell nil)
(setq-default use-dialog-box nil)

;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)
(add-to-list 'default-frame-alist `(font . ,*font))
(add-to-list 'default-frame-alist '(alpha 98 95))   ; *slightly* transparent window

(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))
