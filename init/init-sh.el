(after "popwin"
  (push '("*ansi-term*" :position bottom :height 0.45 :stick t) popwin:special-display-config)
  (push '("*terminal*" :position bottom :height 0.45 :stick t) popwin:special-display-config)
  (push '("*Async Shell Command*" :position bottom) popwin:special-display-config))

(my--cleanup-buffers-add "^\\*Shell Command Output\\*$")
(my--cleanup-buffers-add "^\\*Async Shell Command\\*$")

;; Make shell scrips executable on save?
;; (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Color in *Shell Command Output*
(require 'ansi-color)
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))


(provide 'init-sh)
;;; init-sh.el ends here
