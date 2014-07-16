
;;;; UI Behavior ;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen t)

(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Dynamic linum with +1 padding
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string (+ w 1)) "d "))) ad-do-it))


;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;

; (set-face-attribute 'default t :font 'my-font )
(add-to-list 'default-frame-alist `(font . ,my-font))
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 75))
(add-to-list 'default-frame-alist '(alpha 98 95))   ; *slightly* transparent window

; (set-face-attribute 'mode-line nil :box '(:line-width 4 :color "#1f2g2a" ))

(if window-system (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)

    ; Use system clipboard
    (setq x-select-enable-clipboard t)
    (setq-default line-spacing 1)
    (setq ring-bell-function 'ignore)
) (menu-bar-mode -1))

;;
(provide 'core-ui)
