;;; tools/gist/autoload/evil.el

;;;###autoload (autoload '+gist:send "tools/gist/autoload/evil" nil t)
(evil-define-operator +gist:send (beg end search bang)
  :type inclusive :repeat nil
  (interactive "<r><!>")
  (if bang
      (gist-region-or-buffer-private)
    (gist-region-or-buffer)))

;;;###autoload (autoload '+gist:list "tools/gist/autoload/evil" nil t)
(evil-define-command +gist:list (&optional username)
  (interactive "<a>")
  (if username
      (gist-list-user username)
    (gist-list)))
