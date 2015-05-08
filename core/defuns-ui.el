(eval-when-compile (require 'cl))

(defvar my/dark-theme-p t)
(defvar my/cycle-font-i 0)

;;;###autoload
(defun load-dark-theme()
  (interactive)
  (load-theme *dark-theme t))

;;;###autoload
(defun load-light-theme()
  (interactive)
  (load-theme *light-theme t))

;;;###autoload
(defun toggle-transparency ()
  (interactive)
  (let* ((alpha (frame-parameter nil 'alpha))
         (alpha-val (if (listp alpha) (car alpha) alpha)))
    (if (/= alpha-val 97)
        (set-frame-parameter nil 'alpha 97)
      (set-frame-parameter nil 'alpha 0))))

;;;###autoload
(defun toggle-theme ()
  (interactive)
  (if my/dark-theme-p
      (load-light-theme)
    (load-dark-theme)))

;;;###autoload
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

