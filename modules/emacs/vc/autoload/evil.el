;;; emacs/vc/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+vc:git-browse "emacs/vc/autoload/evil" nil t)
(evil-define-command +vc:git-browse (bang)
  "Ex interface to `+vc/git-browse'."
  (interactive "<!>")
  (+vc/git-browse bang))
