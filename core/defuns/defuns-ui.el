;;; defuns-ui.el

;;;###autoload (autoload 'doom:toggle-fullscreen "defuns-ui" nil t)
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
      (visual-fill-column-mode 1)))

  (evil-define-command doom:toggle-fullscreen ()
    (interactive)
    (set-frame-parameter nil 'fullscreen (if (not (frame-parameter nil 'fullscreen)) 'fullboth))))

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

;;;###autoload
(defun doom/eldoc-show-in-mode-line (input)
  "Display string STR in the mode-line next to minibuffer."
  (with-current-buffer (eldoc-current-buffer)
    (let* ((max              (window-width (selected-window)))
           (str              (and (stringp input) (concat " " input)))
           (len              (length str))
           (tmp-str          str)
           (mode-line-format (or (and str `(:eval (spaceline-ml-eldoc)))
                                 mode-line-format))
           roll mode-line-in-non-selected-windows)
      (catch 'break
        (if (and (> len max) eldoc-mode-line-rolling-flag)
            (progn
              (while (setq roll (sit-for 0.3))
                (setq tmp-str (substring tmp-str 2)
                      mode-line-format (concat tmp-str " [<]" str))
                (force-mode-line-update)
                (when (< (length tmp-str) 2) (setq tmp-str str)))
              (unless roll
                (when eldoc-mode-line-stop-rolling-on-input
                  (setq eldoc-mode-line-rolling-flag nil))
                (throw 'break nil)))
          (force-mode-line-update)
          (sit-for eldoc-show-in-mode-line-delay))))
    (force-mode-line-update)))

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
