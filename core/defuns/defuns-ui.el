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
(after! evil
  (evil-define-command narf:toggle-fullscreen (&optional bang)
    (interactive "<!>")
    (if bang
        (writeroom-mode (if writeroom-mode -1 1))
      (set-frame-parameter nil 'fullscreen (if (not (frame-parameter nil 'fullscreen)) 'fullboth)))))

;;;###autoload
(defun narf/reset-theme ()
  (interactive)
  (narf/load-theme (or narf-current-theme narf-theme)))

;;;###autoload
(defun narf/load-font (font)
  (interactive)
  (set-frame-font font t)
  (setq narf-current-font font))

;;;###autoload
(defun narf/load-theme (theme &optional suppress-font)
  (interactive)
  (when narf-current-theme
    (disable-theme narf-current-theme))
  (load-theme theme t)
  (unless suppress-font
    (narf/load-font narf-current-font))
  (setq narf-current-theme theme))

;;;###autoload
(defun narf/show-as (how &optional pred)
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (ok (or (not pred) (funcall pred beg end))))
    (when ok
      (compose-region beg end how 'decompose-region))
    nil))

(provide 'defuns-ui)
;;; defuns-ui.el ends here
