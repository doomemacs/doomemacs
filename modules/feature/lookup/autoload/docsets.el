;;; feature/lookup/autoload/docsets.el -*- lexical-binding: t; -*-
;;;###if (featurep! +docsets)

(defvar +lookup-docset-alist nil
  "An alist mapping major and minor modes to lists of Dash docsets.

Entries are added by `set-docsets!' and used by `+lookup-docsets-for-buffer' to
assemble a list of installed & active docsets.")

;;;###autodef
(defun set-docsets! (modes &rest docsets)
  "Registers a list of DOCSETS (strings) for MODES (either one major/minor mode
symbol or a list of them). DOCSETS can also contain sublists.

If MODES is a minor mode, you can use :add or :remove as the first element of
DOCSETS, to instruct it to append (or remove) those from the docsets already set
by a major-mode, if any.

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (if (null docsets)
        (setq +lookup-docset-alist
              (delq (assq mode +lookup-docset-alist)
                    +lookup-docset-alist)))
    (setf (alist-get mode +lookup-docset-alist)
          (mapcan #'doom-enlist docsets))))

;;;###autodef
(defalias 'set-docset! #'set-docsets!)

;; FIXME obsolete :docset
;;;###autoload
(def-setting! :docset (modes &rest docsets)
  :obsolete set-docset!
  `(set-docsets! ,modes ,@docsets))


;;
;; Library
;;

;;;###autoload
(defun +lookup-docsets-for-buffer ()
  "Return list of installed & selected docsets for the current major mode.

This list is built from `+lookup-docset-alist'."
  (let ((base-docsets (cdr (assq major-mode +lookup-docset-alist))))
    (dolist (spec +lookup-docset-alist)
      (cl-destructuring-bind (mode . docsets) spec
        (when (and (boundp mode) (symbol-value mode))
          (pcase (car docsets)
            (:add (nconc base-docsets (cdr docsets)))
            (:remove
             (dolist (docset (cdr docsets))
               (setq base-docsets (delete docset base-docsets))))
            (_ (setq base-docsets docsets))))))
    base-docsets))

;;;###autoload
(defun +lookup-docset-installed-p (docset)
  "Return t if DOCSET is installed."
  (let ((path (helm-dash-docsets-path)))
    (file-directory-p
     (expand-file-name (format "%s.docset" docset)
                       path))))

;;;###autoload
(autoload 'helm-dash-installed-docsets "helm-dash")

;;;###autoload
(autoload 'helm-dash-docset-installed-p "helm-dash")


;;
;; Commands
;;

;;;###autoload
(defalias '+lookup/install-docset #'helm-dash-install-docset)

(defvar counsel-dash-docsets)
(defvar helm-dash-docsets)
;;;###autoload
(defun +lookup/in-docsets (&optional query docsets)
  "Lookup QUERY in dash DOCSETS.

QUERY is a string and docsets in an array of strings, each a name of a Dash
docset. Requires either helm or ivy.

Use `+lookup/install-docset' to install docsets."
  (interactive)
  (let* ((counsel-dash-docsets (or docsets (+lookup-docsets-for-buffer)))
         (helm-dash-docsets counsel-dash-docsets)
         (query (or query (+lookup--symbol-or-region) "")))
    (cond ((featurep! :completion helm)
           (helm-dash query))
          ((featurep! :completion ivy)
           (counsel-dash query))
          ((user-error "No dash backend is installed, enable ivy or helm.")))))
