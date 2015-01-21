(eval-when-compile (require 'cl))

(defvar my/dark-theme-p t)
(defvar my/cycle-font-i 0)

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
  (if (/= (frame-parameter nil 'alpha) 96)
      (set-frame-parameter nil 'alpha 96)
    (set-frame-parameter nil 'alpha 0)))

;;;###autoload
(defun toggle-theme ()
  (interactive)
  (if my/dark-theme-p
      (load-light-theme)
    (load-dark-theme)))

;;;###autoload
(defun cycle-font (&optional i)
  "Cycle between fonts specified in *fonts in init.el"
  (interactive)
  (if (numberp i)
      (setq my/cycle-font-i i)
    (if (>= my/cycle-font-i (1- (length *fonts)))
        (setq my/cycle-font-i 0)
      (cl-incf my/cycle-font-i)))
  (let* ((font (nth my/cycle-font-i *fonts))
         (font-name (nth 0 font))
         (font-size (nth 1 font))
         (font-aa (nth 2 font)))
    (unless (member font-name (font-family-list))
      (error "Font %s isn't installed" font-name))
    (let ((font-str (concat font-name "-" (number-to-string font-size))))
      (add-to-list 'default-frame-alist `(font . ,font-str))
      (add-to-list 'initial-frame-alist `(font . ,font-str)))
    (load-font font-name font-size)
    (setq ns-antialias-text font-aa)))
