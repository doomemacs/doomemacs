;; Inspired by http://demonastery.org/2013/04/emacs-evil-narrow-region/
;;;###autoload
(defun my-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))


;; Killing Buffers ;;;;;;;;;;;;;;;;;;;;;
;; Buffer defuns
(defvar my-cleanup-buffers-alist '("^ \\*"
                                  "^\\*Backtrace\\*$"
                                  "^\\*Warnings\\*$"
                                  "^\\*Compile-Log\\*$"
                                  "^\\*Ediff.*\\*$"
                                  help-mode
                                  dired-mode
                                  reb-mode)
  "A list of buffer name regexps or major-mode symbols. If buried buffers
  match/have that mode active, `cleanup-buffers' will kill them.")

;;;###autoload
(defun my--cleanup-buffers-add (regexp)
  (add-to-list 'my-cleanup-buffers-alist regexp))

;;;###autoload
(defun my-cleanup-buffers ()
  "Kill left-over temporary, dired or buried special buffers"
  (interactive)
  (let* ((this-frame (selected-frame))
         (kill-list (buffer-list this-frame)))
    (setq kill-list
          (-filter (lambda (b)
                     (unless (get-buffer-window b) ; don't kill if visible
                       (-any? (lambda (pred)
                                (cond ((stringp pred)
                                       (s-matches? pred (buffer-name b)))
                                      ((symbolp pred)
                                       (eq (buffer-local-value 'major-mode b) pred))))
                              my-cleanup-buffers-alist)))
                   kill-list))

    (message "Cleaned up %s buffers" (length kill-list))
    (mapc 'kill-buffer kill-list)))

;;;###autoload
(defun my-kill-matching-buffers (regexp &optional buffer-list)
  (interactive)
  (mapc (lambda (b)
          (if (s-matches? regexp (buffer-name b))
              (kill-buffer b)))
        (if buffer-list buffer-list (buffer-list))))
