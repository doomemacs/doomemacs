;;; term/shell/autoload.el -*- lexical-binding: t; -*-

(defun +shell-idle-p (buf)
  "Return t if the shell in BUF is not running something.
When available, use process hierarchy information via pstree for
local shells.  Otherwise, we ask comint if the point is after a
prompt."
  (with-current-buffer buf
    (let ((comint-says-idle (and
                             (> (point) 1) ;; if point > 1
                             ;; see if previous char has the prompt face
                             (equal '(comint-highlight-prompt)
                                    (get-text-property
                                     (- (point) 1) 'font-lock-face)))))
      (if (file-remote-p default-directory)
          ;; for remote shells we have to rely on comint
          comint-says-idle
        ;; for local shells, we can potentially do better using pgrep
        (condition-case nil
            (case (call-process ;; look at the exit code of pgrep -P <pid>
                   "pgrep" nil nil nil "-P"
                   (number-to-string (process-id (get-buffer-process buf))))
              (0 nil) ;; child procxesses found, not idle
              (1 t)   ;; not running any child processes, it's idle
              (t comint-says-idle)) ;; anything else, fall back on comint.
          (error comint-says-idle)))))) ;; comint fallback if execution failed

(defun +shell-unused-buffer ()
  "TODO"
  (or (cl-find-if #'+shell-idle-p (doom-buffers-in-mode 'shell-mode))
      (generate-new-buffer "*doom:shell*")))

(defun +shell-tramp-hosts ()
  "Ask tramp for a list of hosts that we can reach through ssh."
  (cl-reduce #'append
             (mapcar (lambda (x)
                       (delq nil (mapcar #'cadr (apply (car x) (cdr x)))))
                     (tramp-get-completion-function "scp"))))

(defun +shell--sentinel (process _event)
  (when (memq (process-status process) '(exit stop))
    (kill-buffer (process-buffer process))))


;;;###autoload
(defun +shell/toggle (&optional command)
  "Toggle a persistent terminal popup window.

If popup is visible but unselected, selected it.
If popup is focused, kill it."
  (interactive)
  (let ((buffer
         (get-buffer-create
          (format "*doom:shell-popup:%s*"
                  (if (bound-and-true-p persp-mode)
                      (safe-persp-name (get-current-persp))
                    "main"))))
        (dir default-directory))
    (if-let (win (get-buffer-window buffer))
        (if (eq (selected-window) win)
            (let (confirm-kill-processes)
              (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
              (delete-window win)
              (ignore-errors (kill-buffer buffer)))
          (select-window win)
          (when (bound-and-true-p evil-local-mode)
            (evil-change-to-initial-state))
          (goto-char (point-max)))
      (with-current-buffer (pop-to-buffer buffer)
        (if (not (eq major-mode 'shell-mode))
            (shell buffer)
          (run-mode-hooks 'shell-mode-hook)
          (cd dir))
        (let ((process (get-buffer-process (current-buffer))))
          (set-process-sentinel process #'+shell--sentinel)
          (when command
            (comint-send-string process command)))))))

;;;###autoload
(defun +shell/here (&optional command)
  "Open a terminal buffer in the current window.

If already in a shell buffer, clear it and cd into the current directory."
  (interactive)
  (let ((buffer (+shell-unused-buffer))
        (dir default-directory))
    (with-current-buffer (switch-to-buffer buffer)
      (if (not (eq major-mode 'shell-mode))
          (shell buffer)
        (erase-buffer)
        (cd dir))
      (let ((process (get-buffer-process (current-buffer))))
        (set-process-sentinel process #'+shell--sentinel)
        (when command
          (comint-send-string process command))))
    buffer))


;; TODO +shell/frame -- dedicate current frame to shell buffers
;; TODO +shell/frame-quite -- revert frame to before +term/frame
