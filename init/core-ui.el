(require-packages
 '(uniquify                ; unique buffer names for identical filenames
   diminish                ; shrinks/removes modeline elements
   ))

(setq inhibit-startup-screen t)     ; don't show EMACs start screen
(fset 'yes-or-no-p 'y-or-n-p)       ; y/n instead of yes/no
(global-linum-mode t)               ; line numbers for everybody!

;; Shrink mode-line
(add-hook 'emacs-startup-hook
    (lambda()
      (diminish 'autopair-mode)
      (diminish 'anzu-mode)
      (diminish 'volatile-highlights-mode)
      (diminish 'undo-tree-mode)
      (diminish 'auto-complete-mode)
      (diminish 'highlight-indentation-mode)
      (diminish 'flyspell-mode " ?")
      ))

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

;;;#uniquify
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;;;; GUI Settings ;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist `(font . ,my-font))
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 75))
(add-to-list 'default-frame-alist '(alpha 98 95))   ; *slightly* transparent window

(if window-system (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)

    ; Use system clipboard
    (setq x-select-enable-clipboard t)
    ; (setq-default line-spacing 1)
    (setq ring-bell-function 'ignore)
) (menu-bar-mode -1))

;;
(provide 'core-ui)
