;;; emacs/eshell/autoload/evil.el

;;;###autoload (autoload '+eshell:run "emacs/eshell/autoload/evil" nil t)
(evil-define-command +eshell:run (command bang)
  (interactive "<fsh><!>")
  (if bang
      (+eshell/run)
    (+eshell/popup)))

