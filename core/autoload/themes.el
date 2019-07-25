;;; ../work/conf/doom-emacs/core/autoload/themes.el -*- lexical-binding: t; -*-

(defun doom--custom-theme-set-face (spec)
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect `(,face ,(cdr spec))))
        ((keywordp (car spec))
         `((,(car spec) ((t ,(cdr spec))))))
        (`((,(car spec) ,(cdr spec))))))

;;;###autoload
(defun custom-theme-set-faces! (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME.

THEME can be a single symbol or list thereof. If nil, apply these settings to
all themes. It will apply to all themes once they are loaded.

  (custom-theme-set-faces! '(doom-one doom-one-light)
   `(mode-line :foreground ,(doom-color 'blue))
   `(mode-line-buffer-id :foreground ,(doom-color 'fg) :background \"#000000\")
   '(mode-line-success-highlight :background \"#00FF00\")
   '(org-tag :background \"#4499FF\")
   '(org-ellipsis :inherit org-tag)
   '(which-key-docstring-face :inherit font-lock-comment-face))"
  `(let* ((themes (doom-enlist (or theme 'user)))
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

SPECS is a list of face specs.

This is a drop-in replacement for `custom-set-face' that allows for a simplified
face format, e.g.

  (custom-set-faces!
   `(mode-line :foreground ,(doom-color 'blue))
   `(mode-line-buffer-id :foreground ,(doom-color 'fg) :background \"#000000\")
   '(mode-line-success-highlight :background \"#00FF00\")
   '(org-tag :background \"#4499FF\")
   '(org-ellipsis :inherit org-tag)
   '(which-key-docstring-face :inherit font-lock-comment-face))"
  (apply #'custom-theme-set-faces! 'user specs))

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
