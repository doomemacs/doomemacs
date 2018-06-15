;;; feature/lookup/autoload/devdocs.el -*- lexical-binding: t; -*-
;;;###if (featurep! +devdocs)

;;;###autodef
(defun set-devdocs! (modes docset)
  "Map major MODES (one major-mode symbol or a list of them) to a devdocs
DOCSET (a string).

See `devdocs-alist' for the defaults. "
  (after! (:when (boundp 'devdocs-alist))
    (map-put devdocs-alist mode docsets)))

;;;###autoload
(def-setting! :devdocs (modes docset)
  "Map major MODES (one major-mode symbol or a list of them) to a devdocs
DOCSET (a string).

See `devdocs-alist' for the defaults. "
  :obsolete set-devdocs!
  `(set-devdocs! ,modes ,docset))
