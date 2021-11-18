;;; email/mu4e/autoload/mu-lock.el -*- lexical-binding: t; -*-
(autoload 'file-notify-rm-watch "filenotify")
(autoload 'file-notify-add-watch "filenotify")

(defvar +mu4e-lock-file (expand-file-name "mu4e_lock" temporary-file-directory)
  "Location of the lock file which stores the PID of the process currenty running mu4e")
(defvar +mu4e-lock-request-file (expand-file-name "mu4e_lock_request" temporary-file-directory)
  "Location of the lock file for which creating indicated that another process wants the lock to be released")

(defvar +mu4e-lock-greedy nil
  "Whether to 'grab' the `+mu4e-lock-file' if nobody else has it, i.e. start Mu4e")
(defvar +mu4e-lock-relaxed t
  "Whether if someone else wants the lock (signaled via `+mu4e-lock-request-file'), we should stop Mu4e and let go of it")

(defun +mu4e-lock-pid-info ()
  "Get info on the PID refered to in `+mu4e-lock-file' in the form (pid . process-attributes)
 If the file or process do not exist, the lock file is deleted an nil returned."
  (when (file-exists-p +mu4e-lock-file)
    (let* ((coding-system-for-read 'utf-8)
           (pid (string-to-number
                 (with-temp-buffer
                   (insert-file-contents +mu4e-lock-file)
                   (buffer-string))))
           (process (process-attributes pid)))
      (if (and process (string-match-p "[Ee]macs" (alist-get 'comm process)))
          (cons pid process)
        (delete-file +mu4e-lock-file) nil))))

(defun +mu4e-lock-available (&optional strict)
  "If the `+mu4e-lock-file' is available (unset or owned by this emacs) return t.
If STRICT only accept an unset lock file."
  (not (when-let* ((lock-info (+mu4e-lock-pid-info))
                   (pid (car lock-info)))
         (when (or strict (/= (emacs-pid) pid)) t))))

;;;###autoload
(defun +mu4e-lock-file-delete-maybe ()
  "Check `+mu4e-lock-file', and delete it if this process is responsible for it."
  (when (+mu4e-lock-available)
    (delete-file +mu4e-lock-file)
    (file-notify-rm-watch +mu4e-lock--request-watcher)))

;;;###autoload
(defun +mu4e-lock-start (orig-fun &optional callback)
  "Check `+mu4e-lock-file', and if another process is responsible for it, abort starting.
Else, write to this process' PID to the lock file"
  (unless (+mu4e-lock-available)
    (call-process "touch" nil nil nil +mu4e-lock-request-file)
    (message "Lock file exists, requesting that it be given up")
    (sleep-for 0.1)
    (delete-file +mu4e-lock-request-file))
  (if (not (+mu4e-lock-available))
      (user-error "Unfortunately another Emacs is already doing stuff with Mu4e, and you can only have one at a time")
    (write-region (number-to-string (emacs-pid)) nil +mu4e-lock-file)
    (delete-file +mu4e-lock-request-file)
    (call-process "touch" nil nil nil +mu4e-lock-request-file)
    (funcall orig-fun callback)
    (setq +mu4e-lock--request-watcher
          (file-notify-add-watch +mu4e-lock-request-file
                                 '(change)
                                 #'+mu4e-lock-request))))

(defvar +mu4e-lock--file-watcher nil)
(defvar +mu4e-lock--file-just-deleted nil)
(defvar +mu4e-lock--request-watcher nil)

(defun +mu4e-lock-add-watcher ()
  (setq +mu4e-lock--file-just-deleted nil)
  (file-notify-rm-watch +mu4e-lock--file-watcher)
  (setq +mu4e-lock--file-watcher
        (file-notify-add-watch +mu4e-lock-file
                               '(change)
                               #'+mu4e-lock-file-updated)))

(defun +mu4e-lock-request (event)
  "Handle another process requesting the Mu4e lock."
  (when (equal (nth 1 event) 'created)
    (when +mu4e-lock-relaxed
      (mu4e~stop)
      (file-notify-rm-watch +mu4e-lock--file-watcher)
      (message "Someone else wants to use Mu4e, releasing lock")
      (delete-file +mu4e-lock-file)
      (run-at-time 0.2 nil #'+mu4e-lock-add-watcher))
    (delete-file +mu4e-lock-request-file)))

(defun +mu4e-lock-file-updated (event)
  (if +mu4e-lock--file-just-deleted
      (+mu4e-lock-add-watcher)
    (when (equal (nth 1 event) 'deleted)
      (setq +mu4e-lock--file-just-deleted t)
      (when (and +mu4e-lock-greedy (+mu4e-lock-available t))
        (message "Noticed Mu4e lock was available, grabbed it")
        (run-at-time 0.2 nil #'mu4e~start)))))
