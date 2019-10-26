;;; lang/org/autoload/org-export.el -*- lexical-binding: t; -*-

(defun +org--yank-html-buffer (buffer)
  (with-current-buffer buffer
    (require 'ox-clip)
    (cond ((or IS-WINDOWS IS-MAC)
           (shell-command-on-region
            (point-min)
            (point-max)
            (cond (IS-WINDOWS ox-clip-w32-cmd)
                  (IS-MAC     ox-clip-osx-cmd))))
          (IS-LINUX
           (let ((html (buffer-string)))
             (with-temp-file (make-temp-file "ox-clip-md" nil ".html")
               (insert html))
             (apply #'start-process "ox-clip" "*ox-clip*"
                    (split-string ox-clip-linux-cmd " ")))))))


;;
;;; Commands

;;;###autoload
(defun +org/export-to-clipboard (backend)
  "Exports the current buffer/selection to the clipboard.

Prompts for what BACKEND to use. See `org-export-backends' for options."
  (interactive
   (list (intern (completing-read "Export to: " org-export-backends))))
  (let ((buffer (org-export-to-buffer backend "*Formatted Copy*" nil nil t t)))
    (unwind-protect
        (with-current-buffer buffer
          (kill-new (buffer-string)))
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun +org/export-to-clipboard-as-rich-text (beg end)
  "Export the current buffer to HTML then copies it to clipboard as rich text.

Supports org-mode, markdown-mode, and gfm-mode buffers. In any other mode,
htmlize is used (takes what you see in Emacs and converts it to html, text
properties and font-locking et all)."
  (interactive "r")
  (pcase major-mode
    ((or `markdown-mode `gfm-mode)
     (+org--yank-html-buffer (markdown)))
    (_ (ox-clip-formatted-copy beg end))))
