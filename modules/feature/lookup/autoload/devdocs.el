;;; feature/lookup/autoload/devdocs.el -*- lexical-binding: t; -*-
;;;###if (featurep! +devdocs)

;;;###autoload
(def-setting! :devdocs (modes docset)
  "Map major MODES (one major-mode symbol or a list of them) to a devdocs
DOCSET (a string).

See `devdocs-alist' for the defaults. "
  `(dolist (mode ',modes)
     (push (cons mode ,docset) devdocs-alist)))
