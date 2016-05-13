;;; defuns-ui.el
;; for ../core-ui.el

;;;###autoload (autoload 'narf:toggle-fullscreen "defuns-ui" nil t)
;;;###autoload (autoload 'narf:set-columns "defuns-ui" nil t)
(after! evil
  (evil-define-command narf:set-columns (&optional bang columns)
    "Adjusts visual-fill-column-width on the fly."
    (interactive "<!><a>")
    (if (or (= (length columns) 0) bang)
        (progn
          (setq visual-fill-column-width 80)
          (when visual-fill-column-mode
            (visual-fill-column-mode -1)))
      (setq columns (string-to-number columns))
      (when (> columns 30)
        (setq visual-fill-column-width columns)))
    (if visual-fill-column-mode
        (visual-fill-column--adjust-window)
      (visual-fill-column-mode 1)))

  (evil-define-command narf:toggle-fullscreen ()
    (interactive)
    (set-frame-parameter nil 'fullscreen (if (not (frame-parameter nil 'fullscreen)) 'fullboth))))

;;;###autoload
(defun narf/reset-theme ()
  (interactive)
  (narf/load-theme (or narf-current-theme narf-default-theme)))

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

;;;###autoload
(defun narf/imenu-list-quit ()
  (interactive)
  (quit-window)
  (mapc (lambda (b) (with-current-buffer b
                 (when imenu-list-minor-mode
                   (imenu-list-minor-mode -1))))
        (narf/get-visible-buffers (narf/get-real-buffers))))

;;;###autoload
(defun narf|hide-mode-line (&rest _)
  (setq mode-line-format nil))

(provide 'defuns-ui)
;;; defuns-ui.el ends here
