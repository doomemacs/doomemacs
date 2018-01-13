;;; private/calendar/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =calendar ()
  "Activate (or switch to) `calendar' in its workspace."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (+workspace-switch "Calendar" t)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*cfw" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf)) (call-interactively +calendar-open-calendar-function))
  (+workspace/display))

;;;###autoload
(defun +calendar/quit ()
  (interactive)
  (+workspace/delete "Calendar"))

;;;###autoload
(defun +calendar/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   ;; :custom-map cfw:my-cal-map
   :contents-sources
   (list
    (cfw:org-create-source (doom-color 'fg))  ; orgmode source
    )))

