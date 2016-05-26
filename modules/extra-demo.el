;;; extra-demo.el --- -*- no-byte-compile: t; -*-

(defvar powerline-height)

;; This library offers:
;;   + impatient-mode: for broadcasting my emacs session
;;   + big-mode: for enlarged text while screencasting
;;   + TODO integration with reveal.js for presentations
;;   + TODO peer programming collab

;; Big-mode settings
(defconst big-mode-font (font-spec :family "Hack" :size 16))
(defconst big-mode-line-spacing 0)
(defconst big-mode-modeline-height 35)

;;
(use-package impatient-mode
  :commands impatient-mode
  :config (httpd-start))

(defvar big-mode--line-spacing line-spacing)
(defvar big-mode--powerline-height powerline-height)
(define-minor-mode big-mode
  :init-value nil
  :lighter " BIG"
  :global t
  (doom/load-font (if big-mode big-mode-font doom-default-font))
  (setq-default
   powerline-height
   (if big-mode big-mode-modeline-height big-mode--powerline-height)
   line-spacing
   (if big-mode big-mode-line-spacing big-mode--line-spacing))
  (if big-mode
      (progn
        (setq neo-window-width 25)
        (add-hook 'neo-after-create-hook 'doom|text-scale-1))
    (setq neo-window-width 28)
    (remove-hook 'neo-after-create-hook 'doom|text-scale-1)))

(defun doom|text-scale-1 (&rest _) (text-scale-set -1) (setq line-spacing 0))
(defun doom|text-scale+1 (&rest _) (text-scale-set +1))

(provide 'extra-demo)
;;; extra-demo.el ends here
