;;; completion/vertico/autoload/magit.el -*- lexical-binding: t; -*-
;;;###if (featurep! :tools magit)

;;;###autoload
(defun +vertico/embark-magit-status (file)
  "Run `magit-status` on repo containing the embark target."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))
