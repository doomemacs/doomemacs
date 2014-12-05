(eval-when-compile (require 'cl))

(defvar my/dark-theme-p t)
(defvar my/presentation-mode-p nil)

;;;###autoload
(defun load-dark-theme()
  (interactive)
  ;; (sml/apply-theme 'respectful)
  (load-theme *dark-theme t))

;;;###autoload
(defun load-light-theme()
  (interactive)
  ;; (sml/apply-theme 'light)
  (load-theme *light-theme t))

;;;###autoload
(defun load-font (font size)
  (interactive)
  (when window-system
    (let ((font-str (concat font "-" (number-to-string size))))
      (if (member font (font-family-list))
          (set-frame-font font-str t t)
        (error "Font %s not installed" font)))))

;;;###autoload
(defun toggle-transparency ()
  (interactive)
  (if (/= (frame-parameter nil 'alpha) 100)
      (set-frame-parameter nil 'alpha 100)
    (set-frame-parameter nil 'alpha 0)))

;;;###autoload
(defun toggle-theme ()
  (interactive)
  (if my/dark-theme-p
      (load-light-theme)
    (load-dark-theme)))

;;;###autoload
(defun toggle-presentation-mode ()
  (interactive)
  (if my/presentation-mode-p
      (load-font *default-font *default-font-size)
    (load-font *presentation-font *presentation-font-size))
  (setq my/presentation-mode-p (not my/presentation-mode-p)))
