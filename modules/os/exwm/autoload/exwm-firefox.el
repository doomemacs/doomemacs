;;; private/exwm/autoload/exwm-firefox.el -*- lexical-binding: t; -*-

;;;###autoload
(defun exwm-firefox-core-hint-links ()
  "Select and open a link with your keyboard."
  (interactive)
  (exwm-input--fake-key ?\M-j)
  (exwm-firefox-evil-insert))

;;;###autoload
(defun exwm-firefox-core-hint-links-new-tab-and-switch ()
  "Select and open a link in a new tab using your keyboard."
  (interactive)
  (exwm-input--fake-key ?\M-l)
  (exwm-firefox-evil-insert))
