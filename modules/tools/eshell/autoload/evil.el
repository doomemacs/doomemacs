;;; tools/eshell/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+eshell:run "tools/eshell/autoload/evil" nil t)
(evil-define-command +eshell:run (command bang)
  ;; TODO Add COMMAND support
  (interactive "<fsh><!>")
  (if bang
      (+eshell/open command)
    (+eshell/open-popup command)))

