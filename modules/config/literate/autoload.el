;;; config/literate/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defalias '+literate/reload #'doom/reload)

;;;###autoload
(defun +literate-recompile-maybe-h ()
  "Recompile config.org if we're editing an org file in our DOOMDIR.

We assume any org file in `doom-private-dir' is connected to your literate
config, and should trigger a recompile if changed."
  (when (and (eq major-mode 'org-mode)
             (file-in-directory-p buffer-file-name doom-private-dir))
    (+literate-tangle 'force)))
