;;; ui/unicode/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook! 'after-setting-font-hook :depth -90
  (defun +unicode-init-fonts-h ()
    "Set up `unicode-fonts' to eventually run; accommodating the daemon, if
necessary."
    (setq-default bidi-display-reordering t)
    (+unicode-setup-fonts-h (selected-frame))))

;; From font-utils.el, required by unicode-fonts.el.
;; This only marks the variable as special locally.
(defvar font-utils-use-memory-cache)

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
      (let ((inhibit-redisplay nil)
            (inhibit-message nil)
            ;; font-utils says "`font-family-list' often gives truncated results
            ;; before Emacs is fully initialized". That is irrelevant when
            ;; `unicode-fonts' is testing for the existence of fonts in a loop,
            ;; all calls during the same phase in startup and thus would see the
            ;; same set of available fonts regardless of caching.
            (font-utils-use-memory-cache t))
        (unicode-fonts-setup)))))
