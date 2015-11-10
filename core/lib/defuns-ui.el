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
    (set-frame-parameter nil 'fullscreen (if (not (frame-parameter nil 'fullscreen)) 'fullboth))))

;;;###autoload
(defvar narf--write-mode nil)
(defun narf:toggle-write-mode ()
  (interactive)
  (require 'writeroom-mode)
  (let ((writeroom-width 100)
        (writeroom-extra-line-spacing 12))
    (writeroom-mode (if writeroom-mode -1 1))
    (setq narf--write-mode writeroom-mode)

    ;; (setq truncate-lines (not narf--write-mode))
    ;; (setq word-wrap narf--write-mode)
    (variable-pitch-mode narf--write-mode)

    (if narf--write-mode
        (narf/load-theme 'solarized-light)
      (narf/reset-theme))

    (fringe-mode (if narf--write-mode 0 '(3 . 6)))
    (auto-fill-mode (if narf--write-mode -1 +1))
    (text-scale-set (if narf--write-mode 2.5 0))
    (scroll-bar-mode (if narf--write-mode 1 -1))

    (when IS-MAC
      (setq ;; sane trackpad/mouse scroll settings
       mac-mouse-wheel-smooth-scroll narf--write-mode
       mouse-wheel-progressive-speed narf--write-mode))

    (when (eq major-mode 'org-mode)
      (org-indent-mode (if narf--write-mode -1 1)))))

(defvar narf--big-mode nil)
;;;###autoload
(defun narf:toggle-big-mode ()
  (interactive)
  (set-frame-font (if narf--big-mode narf-default-font narf-big-font))
  (setq narf--big-mode (not narf--big-mode)))

;;;###autoload
(defun narf/reset-theme ()
  (interactive)
  (narf/load-theme 'narf-dark))

;;;###autoload
(defun narf/load-theme (theme)
  (interactive)
  (mapc (lambda (th)
          (when (custom-theme-enabled-p th) (disable-theme th)))
        custom-enabled-themes)
  (load-theme theme t))

;;;###autoload
(defun narf/default-font ()
  (interactive)
  (set-frame-font narf-default-font))

;;;###autoload
(defun narf/big-font ()
  (interactive)
  (set-frame-font narf-big-font))

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
