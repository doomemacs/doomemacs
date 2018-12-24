;;; feature/lookup/autoload/docsets.el -*- lexical-binding: t; -*-
;;;###if (featurep! +docsets)

(defvar +lookup-docset-alist nil
  "An alist mapping major and minor modes to lists of Dash docsets.

Entries are added by `set-docsets!' and used by `+lookup-docsets-for-buffer' to
assemble a list of installed & active docsets.")

;;;###autodef
(defun set-docsets! (modes &rest docsets)
  "Registers a list of DOCSETS for MODES.

MODES can be one major mode, or a list thereof.

DOCSETS can be strings, each representing a dash docset, or a vector with the
structure [DOCSET FORM]. If FORM evaluates to nil, the DOCSET is omitted. If it
is non-nil, (format DOCSET FORM) is used as the docset.

The first element in DOCSETS can be :add or :remove, making it easy for users to
add to or remove default docsets from modes.

DOCSETS can also contain sublists, which will be flattened.

Example:

  (set-docsets! '(js2-mode rjsx-mode) \"JavaScript\"
    [\"React\" (eq major-mode 'rjsx-mode)]
    [\"TypeScript\" (bound-and-true-p tide-mode)])

Used by `+lookup/in-docsets' and `+lookup/documentation'."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (if (null docsets)
        (setq +lookup-docset-alist
              (delq (assq mode +lookup-docset-alist)
                    +lookup-docset-alist))
      (let ((action  (if (keywordp (car docsets)) (pop docsets)))
            (docsets (mapcan #'doom-enlist docsets))) ; flatten list
        (setf (alist-get mode +lookup-docset-alist)
              (pcase action
                (:add    (append docsets (alist-get mode +lookup-docset-alist)))
                (:remove (cl-set-difference (alist-get mode +lookup-docset-alist) docsets))
                (_ docsets)))))))

;;;###autodef
(defalias 'set-docset! #'set-docsets!)

;; FIXME obsolete :docset
;;;###autoload
(def-setting! :docset (modes &rest docsets)
  :obsolete set-docset!
  `(set-docsets! ,modes ,@docsets))


;;
;; Library

;;;###autoload
(defun +lookup-docsets-for-buffer ()
  "Return list of installed & selected docsets for the current major mode.

This list is built from `+lookup-docset-alist'."
  (cl-loop for docset in (cdr (assq major-mode +lookup-docset-alist))
           when (or (stringp docset)
                    (and (vectorp docset)
                         (eval (aref docset 1) t)))
           collect docset))

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
