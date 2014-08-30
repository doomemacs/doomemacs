(global-linum-mode t)   ; line numbers for everybody!

;; Show line/col-no in mode-line
(line-number-mode t)
(column-number-mode t)

;; make the fringe unintrusive
(when (fboundp 'fringe-mode) (fringe-mode 8))

;; Line numbers with +1 left-padding
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string (+ w 1)) "d" " "))) ad-do-it))

;; Show full path in window title
(setq frame-title-format
      '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))

;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)
(add-to-list 'default-frame-alist `(font . ,my/font))
(add-to-list 'default-frame-alist '(alpha 98 95))   ; *slightly* transparent window

(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))

;;
(provide 'core-ui)
