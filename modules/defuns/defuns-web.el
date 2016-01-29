;;; defuns-web.el

;;;###autoload
(defun narf/web-html-email2mailto (beg end)
  (interactive "r")
  (replace-regexp "\\b\\([a-zA-Z0-9._+-%]+@[a-zA-Z0-9-.]+\\.[a-zA-Z]+\\)\\b"
                  "<a href=\"mailto:\\1\">\\1</a>"
                  nil beg end))

;;;###autoload
(defun narf/web-html-url2anchor (beg end)
  (interactive "r")
  (replace-regexp "\\bhttps?://.+?\\b"
                  "<a href=\"\\1\">\\1</a>"
                  nil beg end))

;;;###autoload
(defun narf/web-refresh-browser ()
  (interactive)
  (call-process-shell-command "osascript -e 'tell application \"Google Chrome\" to tell the active tab of its first window to reload' &" nil 0))

(provide 'defuns-web)
;;; defuns-web.el ends here
