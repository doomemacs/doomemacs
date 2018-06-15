;;; feature/lookup/autoload/docsets.el -*- lexical-binding: t; -*-
;;;###if (featurep! +docsets)

(defvar-local helm-dash-docsets nil
  "Docsets to use for this buffer.")

(defvar-local counsel-dash-docsets nil
  "Docsets to use for this buffer.")

;;;###autodef
(defun set-docset! (modes &rest docsets)
  "Registers a list of DOCSETS (strings) for MODES (either one major mode
symbol or a list of them).

If MODES is a minor mode, you can use :add or :remove as the first element of
DOCSETS, to instruct it to append (or remove) those from the docsets already set
by a major-mode, if any.

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  (dolist (mode (doom-enlist modes))
    (let ((hook-sym
           (intern (format "+lookup|%s-docsets--%s"
                           (pcase (car docsets)
                             (:add 'add)
                             (:remove 'remove)
                             (_ 'set))
                           mode))))
      (fset hook-sym
            (lambda ()
              (let (var-sym)
                (cond ((featurep! :completion ivy)
                       (setq var-sym 'counsel-dash-docsets))
                      ((featurep! :completion helm)
                       (setq var-sym 'helm-dash-docsets)))
                (when var-sym
                  (let ((val (symbol-value var-sym)))
                    (pcase (car docsets)
                      (:add
                       (set var-sym (append val (cdr docsets))))
                      (:remove
                       (set var-sym
                            (cl-loop with to-delete = (cdr docsets)
                                     for docset in val
                                     unless (member docset to-delete)
                                     collect docset)))
                      (_ (set var-sym (cdr docsets)))))))))
      (add-hook (intern (format "%s-hook" mode)) hook-sym))))

;;;###autoload
(def-setting! :docset (modes &rest docsets)
  "Registers a list of DOCSETS (strings) for MODES (either one major mode
symbol or a list of them).

If MODES is a minor mode, you can use :add or :remove as the first element of
DOCSETS, to instruct it to append (or remove) those from the docsets already set
by a major-mode, if any.

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  :obsolete set-docset!
  `(set-docset! ,modes ,@docsets))

;;;###autoload
(autoload 'helm-dash-installed-docsets "helm-dash")

;;;###autoload
(autoload 'helm-dash-docset-installed-p "helm-dash")
