;;; ui/unicode/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook! 'after-setting-font-hook :depth -90
  (defun +unicode-init-fonts-h ()
    "Set up `unicode-fonts' to eventually run; accommodating the daemon, if
necessary."
    (setq-default bidi-display-reordering t)
    (+unicode-setup-fonts-h (selected-frame))))

;;;###autoload
(defun +unicode-setup-fonts-h (&optional frame)
  "Initialize `unicode-fonts', if in a GUI session.

If doom-symbol-font is set, add it as a preferred font for all Unicode blocks."
  (when (and frame (display-multi-font-p frame))
    (with-selected-frame frame
      (require 'unicode-fonts)
      (when doom-symbol-font
        (let ((doom-symbol-font-family (plist-get (font-face-attributes doom-symbol-font) :family)))
          (dolist (unicode-block unicode-fonts-block-font-mapping)
            (push doom-symbol-font-family (cadr unicode-block)))))
      ;; NOTE: will impact startup time on first run
      (let (inhibit-redisplay inhibit-message)
        (unicode-fonts-setup)))))
