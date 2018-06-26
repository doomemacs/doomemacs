;;; feature/lookup/autoload/docsets.el -*- lexical-binding: t; -*-
;;;###if (featurep! +docsets)

(defvar-local helm-dash-docsets nil
  "Docsets to use for this buffer.")

(defvar-local counsel-dash-docsets nil
  "Docsets to use for this buffer.")

;;;###autodef
(defun set-docset! (modes &rest docsets)
  "Registers a list of DOCSETS (strings) for MODES (either one major/minor mode
symbol or a list of them).

If MODES is a minor mode, you can use :add or :remove as the first element of
DOCSETS, to instruct it to append (or remove) those from the docsets already set
by a major-mode, if any.

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((fn   (intern (format "+lookup|init-docsets--%s" mode)))
          (hook (intern (format "%s-hook" mode))))
      (cond ((null (car-safe docsets))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset fn
                   (lambda ()
                     (let ((var-sym (if (featurep! :completion ivy)
                                        'counsel-dash-docsets
                                      'helm-dash-docsets)))
                       (set var-sym
                            (append (symbol-value var-sym)
                                    docsets)))))
             (add-hook hook fn))))))

;; FIXME obsolete :docset
;;;###autoload
(def-setting! :docset (modes &rest docsets)
  :obsolete set-docset!
  `(set-docset! ,modes ,@docsets))

;;;###autoload
(autoload 'helm-dash-installed-docsets "helm-dash")

;;;###autoload
(autoload 'helm-dash-docset-installed-p "helm-dash")
