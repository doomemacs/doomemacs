;;; defuns-ui.el

;;;###autoload (autoload 'doom:set-columns "defuns-ui" nil t)
(after! evil
  (evil-define-command doom:set-columns (&optional bang columns)
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
      (visual-fill-column-mode 1))))

;;;###autoload
(defun doom/toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;;;###autoload
(defun doom/reset-theme ()
  (interactive)
  (doom/load-theme (or doom-current-theme doom-default-theme)))

;;;###autoload
(defun doom/load-font (font)
  (interactive)
  (set-frame-font font t)
  (setq doom-current-font font))

;;;###autoload
(defun doom/load-theme (theme &optional suppress-font)
  (interactive)
  (when doom-current-theme
    (disable-theme doom-current-theme))
  (load-theme theme t)
  (unless suppress-font
    (doom/load-font doom-current-font))
  (setq doom-current-theme theme))

;;;###autoload
(defun doom/show-as (how &optional pred)
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (ok (or (not pred) (funcall pred beg end))))
    (when ok
      (compose-region beg end how 'decompose-region))
    nil))

;;;###autoload
(defun doom/imenu-list-quit ()
  (interactive)
  (quit-window)
  (mapc (lambda (b) (with-current-buffer b
                 (when imenu-list-minor-mode
                   (imenu-list-minor-mode -1))))
        (doom/get-visible-buffers (doom/get-real-buffers))))

(put 'doom-hide-mode-line-mode 'permanent-local t)
(put 'doom--mode-line 'permanent-local t)

(defvar doom-hide-mode-line-format nil
  "Format to use when `doom-hide-mode-line-mode' replaces the modeline")

(defvar-local doom--mode-line nil)
;;;###autoload
(define-minor-mode doom-hide-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if doom-hide-mode-line-mode
      (setq doom--mode-line mode-line-format
            mode-line-format doom-hide-mode-line-format)
    (setq mode-line-format doom--mode-line
          doom--mode-line doom-hide-mode-line-format)))

(provide 'defuns-ui)
;;; defuns-ui.el ends here
