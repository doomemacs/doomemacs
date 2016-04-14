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
(defun narf/add-whitespace (&optional start end)
  (interactive (progn (barf-if-buffer-read-only)
                      (if (use-region-p)
                          (list (region-beginning) (region-end))
                        (list nil nil))))
  (save-match-data
    (save-excursion
      (let ((end-marker (copy-marker (or end (point-max))))
            (start (or start (point-min))))
        (goto-char start)
        (while (and (re-search-forward "^$" end-marker t) (not (>= (point) end-marker)))
          (let (line-start line-end)
            (save-excursion
              (forward-line -1)
              (setq line-start (point)
                    line-end   (save-excursion (back-to-indentation) (point)))
              (if (and (= line-start line-end)
                       (/= line-end (line-end-position)))
                  (progn
                    (forward-line 2)
                    (setq line-start (point)
                          line-end   (save-excursion (back-to-indentation) (point)))
                    (forward-line -1))
                (forward-line 1))
              (insert (make-string (- line-end line-start) 32))))
          (forward-line 1)))))
  (set-buffer-modified-p nil)
  nil)

;;;###autoload
(defun narf/imenu-list-quit ()
  (interactive)
  (quit-window)
  (mapc (lambda (b) (with-current-buffer b
                 (when imenu-list-minor-mode
                   (imenu-list-minor-mode -1))))
        (narf/get-visible-buffers (narf/get-real-buffers))))

(provide 'defuns-ui)
;;; defuns-ui.el ends here
