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

;;;###autoload
(defun my--set-region-read-only (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p)))
    (add-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun my--set-region-writeable (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun my-living-buffer-list (&optional buffer-list)
  (-remove 'get-buffer-window (or buffer-list (buffer-list))))


;; Killing Buffers ;;;;;;;;;;;;;;;;;;;;;
;; Buffer defuns
(defvar my-cleanup-buffers-list '("^ \\*"
                                   "^\\*Backtrace\\*$"
                                   "^\\*Warnings\\*$"
                                   "^\\*Compile-Log\\*$"
                                   "^\\*Ediff.*\\*$"
                                   help-mode
                                   image-mode
                                   dired-mode
                                   reb-mode)
  "A list of buffer name regexps or major-mode symbols. If buried buffers
  match/have that mode active, `cleanup-buffers' will kill them.")

(defvar my-cleanup-processes-alist '(("pry" . ruby-mode)
                                     ("irb" . ruby-mode)
                                     ("ipython" . python-mode))
  "An alist of (process-name . major-mode), that `my-cleanup-processes' checks
before killing processes. If there are no buffers with matching major-modes, it
gets killed.")

;;;###autoload
(defun my--cleanup-buffers-add (regexp)
  (add-to-list 'my-cleanup-buffers-list regexp))

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
                              my-cleanup-buffers-list)))
                   kill-list))

    (message "Cleaned up %s buffers" (length kill-list))
    (mapc 'kill-buffer kill-list)

    (my-cleanup-processes)))

;;;###autoload
(defun my-cleanup-processes ()
  (interactive)
  (let ((buffer-list (buffer-list)))
    (dolist (p (process-list))
      (let* ((process-name (process-name p))
             (assoc (assoc process-name my-cleanup-processes-alist)))
        (when (and assoc
                   (not (string= process-name "server"))
                   (process-live-p p)
                   (not (-any? (lambda (b)
                                 (let ((mode (buffer-local-value 'major-mode b)))
                                   (eq mode (cdr assoc))))
                               buffer-list)))
          (message "Cleanup: killing %s" process-name)
          (delete-process p))))))

;;;###autoload
(defun my-kill-matching-buffers (regexp &optional buffer-list)
  (interactive)
  (mapc (lambda (b)
          (if (string-match-p regexp (buffer-name b))
              (kill-buffer b)))
        (if buffer-list buffer-list (buffer-list))))

;; From spacemacs <https://github.com/syl20bnr/spacemacs/blob/master/spacemacs/funcs.el>
;;;###autoload
(defun my-next-real-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (switch-to-next-buffer)
  (let ((i 0))
    (while (and (< i 100) (string-equal "*" (substring (buffer-name) 0 1)))
      (1+ i)
      (switch-to-next-buffer))))

;;;###autoload
(defun my-previous-real-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (switch-to-prev-buffer)
  (let ((i 0))
    (while (and (< i 100) (string-equal "*" (substring (buffer-name) 0 1)))
      (1+ i)
      (switch-to-prev-buffer))))

;;;###autoload
(defun my-kill-real-buffer ()
  "Kill buffer (but only bury scratch buffer)"
  (interactive)
  (let ((bname (buffer-name)))
    (cond ((string-match-p "^\\*scratch\\*" bname)
           (erase-buffer)
           (bury-buffer))
          ((string-equal "*" (substring bname 0 1))
           (previous-buffer))
          (t (kill-this-buffer)))))
