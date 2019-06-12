;;; lang/org/+pomodoro.el -*- lexical-binding: t; -*-
;;;###if (featurep! +pomodoro)

(defun +org-pomodoro/notification(title message)
  "Send a notification"
  (when IS-MAC
    (do-applescript
     (format "display notification \"%s\" with title \"%s\" sound name \"Ping\"" message title))))

(defun +org-pomodoro/pomodoro-notification()
  "show notifications when pomodoro end"
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda ()
                                             (+org-pomodoro/notification "Pomodoro Finished" "Have a break!")))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda ()
                                                         (+org-pomodoro/notification "Short Break" "Ready to Go?")))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda ()
                                                        (+org-pomodoro/notification "Long Break" "Ready to Go?")))))

(def-package! org-pomodoro
  :when (featurep! +pomodoro)
  :after org
  :config
  (+org-pomodoro/pomodoro-notification))
