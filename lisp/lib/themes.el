;;; lisp/lib/themes.el -*- lexical-binding: t; -*-

;;;###autoload
(defconst doom-customize-theme-hook nil)

;;;###autoload
(defun doom--run-customize-theme-hook (fn)
  "Run FN, but suppress any writes to `custom-file'."
  (letf! (defun put (symbol prop value)
           (unless (string-prefix-p "saved-" (symbol-name prop))
             (funcall put symbol prop value)))
    (let (custom--inhibit-theme-enable)
      (funcall fn))))

(add-hook! 'doom-load-theme-hook
  (defun doom-apply-customized-faces-h ()
    "Run `doom-customize-theme-hook'."
    (run-hook-wrapped 'doom-customize-theme-hook #'doom--run-customize-theme-hook)))

(defun doom--normalize-face-spec (spec)
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (car (doom--normalize-face-spec (cons face (cdr spec))))))
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        (`((,(car spec) ,(cdr spec))))))

;;;###autoload
(defmacro custom-theme-set-faces! (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME.

THEME can be a single symbol or list thereof. If nil, apply these settings to
all themes. It will apply to all themes once they are loaded."
  (declare (indent defun))
  (let ((fn (gensym "doom--customize-themes-h-")))
    `(progn
       (defun ,fn ()
         (dolist (theme (ensure-list (or ,theme 'user)))
           (if (or (eq theme 'user)
                   (custom-theme-enabled-p theme))
               (apply #'custom-theme-set-faces theme
                      (mapcan #'doom--normalize-face-spec
                              (list ,@specs))))))
       ;; Apply the changes immediately if the user is using the default theme
       ;; or the theme has already loaded. This allows you to evaluate these
       ;; macros on the fly and customize your faces iteratively.
       (when (or (get 'doom-theme 'history)
                 (null doom-theme))
         (doom--run-customize-theme-hook #',fn))
       ;; FIXME Prevent clobbering this on-the-fly
       (add-hook 'doom-customize-theme-hook #',fn 100))))

;;;###autoload
(defmacro custom-set-faces! (&rest specs)
  "Apply a list of face SPECS as user customizations.

This is a convenience macro alternative to `custom-set-face' which allows for a
simplified face format, and takes care of load order issues, so you can use
doom-themes' API without worry."
  (declare (indent defun))
  `(custom-theme-set-faces! 'user ,@specs))

;;;###autoload
(defun doom/reload-theme ()
  "Reload all currently active themes."
  (interactive)
  (let* ((themes (copy-sequence custom-enabled-themes))
         (real-themes (cl-remove-if-not #'doom--theme-is-colorscheme-p themes)))
    (mapc #'disable-theme themes)
    (mapc #'enable-theme (reverse themes))
    (doom/reload-font)
    (message "%s %s"
             (propertize
              (format "Reloaded %d theme%s:"
                      (length real-themes)
                      (if (cdr real-themes) "s" ""))
              'face 'bold)
             (mapconcat #'prin1-to-string real-themes ", "))))


;;
;;; Helpers

;;;###autoload
(defun doom-theme-face-attribute (theme face attribute &optional recursive)
  "Read a FACE's ATTRIBUTE for a loaded THEME.

This is different from `face-attribute', which reads the attribute of an active
face for the current theme, but an active theme can change (or fail to load) in
non-interactive or frame-less sessions."
  (let* ((spec
          (cl-loop for (type f _ spec) in (get theme 'theme-settings)
                   if (and (eq type 'theme-face) (eq face f))
                   return spec))
         (spec
          (letf! ((defun window-system (_frame) 'x)
                  (defun display-color-cells (_frame) 257)
                  (defun frame-parameter (frame parameter)
                    (pcase parameter
                      (`display-type 'color)
                      (`background-mode 'dark)
                      (_ (funcall frame-parameter frame parameter))))
                  (#'display-supports-face-attributes-p #'always))
            (face-spec-choose spec)))
         (inherit (if recursive (plist-get spec :inherit)))
         (value (if (plist-member spec attribute)
                    (plist-get spec attribute)
                  'unspecified)))
    (when (and inherit (not (eq inherit 'unspecified)))
      (letf! (defun face-attribute (face attribute &optional _frame inherit)
               (doom-theme-face-attribute theme face attribute inherit))
        (setq value (face-attribute-merged-with attribute value inherit))))
    value))

(provide 'doom-lib '(themes))
;;; themes.el ends here
