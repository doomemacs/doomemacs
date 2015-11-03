;;; defuns-ui.el
;; for ../core-ui.el

;;;###autoload
(defun narf:toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha))
         (alpha-val (if (listp alpha) (car alpha) alpha)))
    (if (/= alpha-val 97)
        (set-frame-parameter nil 'alpha 100)
      (set-frame-parameter nil 'alpha 0))))

;;;###autoload (autoload 'narf:toggle-fullscreen "defuns-ui" nil t)
(evil-define-command narf:toggle-fullscreen (&optional bang)
  (interactive "<!>")
  (if bang
      (writeroom-mode (if writeroom-mode -1 1))
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defvar narf--big-mode nil)
;;;###autoload
(defun narf:toggle-big-mode ()
  (interactive)
  (set-frame-font (if narf--big-mode narf-default-font narf-big-font))
  (setq narf--big-mode (not narf--big-mode)))

;;;###autoload
(defun narf/reset-theme ()
  (interactive)
  (load-theme (if window-system narf-theme narf-term-theme) t))

;;;###autoload
(defun narf/default-font ()
  (interactive)
  (set-frame-font narf-default-font))

;;;###autoload
(defun narf/big-font ()
  (interactive)
  (set-frame-font narf-big-font))

(provide 'defuns-ui)
;;; defuns-ui.el ends here
