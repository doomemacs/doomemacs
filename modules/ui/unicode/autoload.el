;;; ui/unicode/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook! 'doom-init-ui-hook
  (defun +unicode-init-fonts-h ()
    "Set up `unicode-fonts' to eventually run; accommodating the daemon, if
necessary."
    (setq-default bidi-display-reordering t
                  doom-unicode-font nil)
    (if (display-graphic-p)
        (+unicode-setup-fonts-h (selected-frame))
      (add-hook 'after-make-frame-functions #'+unicode-setup-fonts-h))))

;;;###autoload
(defun +unicode-setup-fonts-h (&optional frame)
  "Initialize `unicode-fonts', if in a GUI session.

If doom-unicode-font is set, add it as preferred font for all unicode blocks."
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (require 'unicode-fonts)
      (when doom-unicode-font
        (let ((doom-unicode-font-family (plist-get (font-face-attributes doom-unicode-font) :family)))
          (dolist (unicode-block unicode-fonts-block-font-mapping)
            (push doom-unicode-font-family (cadr unicode-block)))))
      ;; NOTE will impact startup time on first run
      (unicode-fonts-setup))))
