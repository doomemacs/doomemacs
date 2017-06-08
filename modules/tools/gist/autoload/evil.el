;;; tools/gist/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+gist:send "tools/gist/autoload/evil" nil t)
(evil-define-operator +gist:send (bang)
  "Create a private gist from the buffer. If BANG then make it public."
  :type inclusive :repeat nil
  (interactive "<!>")
  (if bang
      (gist-region-or-buffer)
    (gist-region-or-buffer-private)))

;;;###autoload (autoload '+gist:list "tools/gist/autoload/evil" nil t)
(evil-define-command +gist:list (&optional username)
  "Pop up a listing of gists."
  (interactive "<a>")
  (if username
      (gist-list-user username)
    (gist-list)))
