;;; lib-plugin.el

(defun narf-lb6-reload ()
  (interactive)
  (let ((dir (f-traverse-upwards (lambda (f) (string-suffix-p ".lbaction" f)))))
    (shell-command (format "open %s" dir))))

(define-minor-mode lb6-mode
  "Launchbar development mode."
  :init-value nil
  :lighter    " lb6"
  :keymap (let ((map (make-sparse-keymap)))
            (map! :map map
                  (:localleader
                    :n "b" 'narf-lb6-reload))
            map)
  (add-yas-minor-mode! 'lb6-mode))
(associate! lb6-mode :match "\\.lb\\(action\\|ext\\)/.+$")

;;

(define-minor-mode hammerspoon-mode
  :init-value nil
  :lighter " hammer"
  :keymap (let ((map ))
            (map! :map map
                  (:localleader
                    :n "b" (λ! (shell-command (format "open hammerspoon://reload")))))
            map)
  (add-yas-minor-mode! 'hammerspoon-mode))
(associate! hammerspoon-mode :match "/\\.?hammerspoon/.+\\.lua$")

(provide 'lib-plugin)
;;; lib-plugin.el ends here

