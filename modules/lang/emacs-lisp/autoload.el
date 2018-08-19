;;; lang/emacs-lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(autoload 'overseer-test "overseer" nil t)


;;
;; Library
;;

;;;###autoload
(defun +emacs-lisp/repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*ielm*")
       (progn (ielm)
              (let ((buf (get-buffer "*ielm*")))
                (bury-buffer buf)
                buf)))))

;;;###autoload
(defun +emacs-lisp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer."
  (require 'pp)
  (let ((result (eval (read (concat "(progn " (buffer-substring-no-properties beg end) "\n)"))))
        (buf (get-buffer-create "*doom eval*"))
        (inhibit-read-only t)
        lines)
    (with-current-buffer buf
      (read-only-mode +1)
      (erase-buffer)
      (setq-local scroll-margin 0)
      (delay-mode-hooks (emacs-lisp-mode))
      (prin1 result buf)
      (pp-buffer)
      (setq lines (count-lines (point-min) (point-max)))
      (cond ((> lines 1)
             (save-selected-window
               (pop-to-buffer buf)
               (with-current-buffer buf
                 (goto-char (point-min)))))
            (t
             (message "%s" (buffer-substring (point-min) (point-max)))
             (kill-buffer buf))))))
