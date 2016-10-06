;;; custom-demo.el --- -*- no-byte-compile: t; -*-

;; This library offers:
;;   + impatient-mode: for broadcasting my emacs session
;;   + big-mode: for enlarged text while screencasting
;;   + TODO integration with reveal.js for presentations
;;   + TODO peer programming collab

;; Big-mode settings
(defconst big-mode-font (font-spec :family "Inconsolata" :size 16))
(defconst big-mode-line-spacing 0)
(defconst big-mode-modeline-height 35)

;;
(use-package impatient-mode
  :commands impatient-mode
  :config (httpd-start))

(defvar big-mode--old-line-spacing line-spacing)
(defvar big-mode--old-modeline-height doom-modeline-height)
(define-minor-mode big-mode
  :init-value nil
  :lighter " BIG"
  :global t
  (when big-mode-font
    (doom/load-font (if big-mode big-mode-font doom-ui-font)))
  (if big-mode
      (setq-default
       doom-modeline-height big-mode-modeline-height
       line-spacing big-mode-line-spacing)
    (setq-default
     doom-modeline-height big-mode--old-modeline-height
     line-spacing big-mode--old-line-spacing)))

(evil-define-command doom:big (&optional size)
  "Use to enable large text mode."
  (interactive "<a>")
  (if size
      (let ((big-mode-font big-mode-font))
        (big-mode -1)
        (font-put big-mode-font :size (string-to-int size))
        (big-mode +1))
    (big-mode (if big-mode -1 +1))))

(defun doom/resize-for-stream ()
  "Resize the frame pixelwise, so that it fits directly into my livecoding.tv
streaming layout."
  (interactive)
  (set-frame-width (selected-frame) 1325 nil t)
  (set-frame-height (selected-frame) 1080 nil t))

(provide 'custom-demo)
;;; custom-demo.el ends here
