;;; feature/lookup/autoload/docsets.el -*- lexical-binding: t; -*-
;;;###if (featurep! +docsets)

;;;###autoload
(def-setting! :docset (modes &rest docsets)
  "Registers a list of DOCSETS (strings) for MODES (either one major mode
symbol or a list of them).

If MODES is a minor mode, you can use :add or :remove as the first element of
DOCSETS, to instruct it to append (or remove) those from the docsets already set
by a major-mode, if any.

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  (let* ((modes (doom-unquote modes))
         (ivy-p (featurep! :completion ivy))
         (hook-sym (intern (format "+lookup|%s-docsets--%s"
                                   (cond ((eq ',(car docsets) :add)    'add)
                                         ((eq ',(car docsets) :remove) 'remove)
                                         ('set))
                                   (string-join docsets "-"))))
         (var-sym (if ivy-p 'counsel-dash-docsets 'helm-dash-docsets)))
    `(progn
       (defun ,hook-sym ()
         (make-variable-buffer-local ',var-sym)
         ,(cond ((eq ',(car docsets) :add)
                 `(setq ,var-sym (append ,var-sym (list ,@(cdr docsets)))))
                ((eq ',(car docsets) :remove)
                 `(setq ,var-sym
                        (cl-loop with to-delete = (list ,@(cdr docsets))
                                 for docset in ,var-sym
                                 unless (member docset to-delete)
                                 collect docset)))
                (`(setq ,var-sym (list ,@docsets)))))
       (add-hook! ,modes #',hook-sym))))

;;;###autoload
(autoload 'helm-dash-installed-docsets "helm-dash")

;;;###autoload
(autoload 'helm-dash-docset-installed-p "helm-dash")
