;;; core/autoload/themes.el -*- lexical-binding: t; -*-

(defun doom--custom-theme-set-face (spec)
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (doom--custom-theme-set-face `(,face ,(cdr spec)))))
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        (`((,(car spec) ,(cdr spec))))))

;;;###autoload
(defun custom-theme-set-faces! (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME.

THEME can be a single symbol or list thereof. If nil, apply these settings to
all themes. It will apply to all themes once they are loaded."
  (let* ((themes (doom-enlist (or theme 'user)))
         (fn (gensym (format "doom--customize-%s-h-" (mapconcat #'symbol-name themes "-")))))
    (fset fn
          (lambda ()
            (dolist (theme themes)
              (when (or (eq theme 'user)
                        (custom-theme-enabled-p theme))
                (apply #'custom-theme-set-faces 'user
                       (mapcan #'doom--custom-theme-set-face
                               specs))))))
    (funcall fn)
    (add-hook 'doom-load-theme-hook fn)))

;;;###autoload
(defun custom-set-faces! (&rest specs)
  "Apply a list of face SPECS as user customizations.

This is a drop-in replacement for `custom-set-face' that allows for a simplified
face format."
  (apply #'custom-theme-set-faces! 'user specs))

(defvar doom--prefer-theme-elc)
;;;###autoload
(defun doom/reload-theme ()
  "Reload the current color theme."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) doom-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (when (and doom-theme (not (memq doom-theme custom-enabled-themes)))
      (let (doom--prefer-theme-elc)
        (load-theme doom-theme t)))
    (doom-init-fonts-h)))
