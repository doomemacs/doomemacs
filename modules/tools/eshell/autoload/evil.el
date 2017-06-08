;;; tools/eshell/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+eshell:run "emacs/eshell/autoload/evil" nil t)
(evil-define-command +eshell:run (_command bang)
  ;; TODO Add COMMAND support
  (interactive "<fsh><!>")
  (if bang
      (+eshell/run)
    (+eshell/popup)))

