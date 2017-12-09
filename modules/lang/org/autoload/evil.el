;;; lang/org/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;; TODO +org-attach:find

;;;###autoload (autoload '+org-attach:uri "lang/org/autoload/evil" nil t)
(evil-define-command +org-attach:uri (uri)
  "Downloads the file at URL and places an org link to it at the cursor."
  (interactive "<f>")
  (+org-attach/uri uri))

