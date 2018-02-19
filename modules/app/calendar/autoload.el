;;; app/calendar/autoload.el -*- lexical-binding: t; -*-

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
  "TODO"
  (interactive)
  (+workspace/delete "Calendar"))

;;;###autoload
(defun +calendar/open-calendar ()
  "TODO"
  (interactive)
  (cfw:open-calendar-buffer
   ;; :custom-map cfw:my-cal-map
   :contents-sources
   (list
    (cfw:org-create-source (doom-color 'fg))  ; orgmode source
    )))

;;;###autoload
(defun +calendar*cfw:render-button (title command &optional state)
    "render-button
 TITLE
 COMMAND
 STATE"
    (let ((text (concat " " title " "))
          (keymap (make-sparse-keymap)))
      (cfw:rt text (if state 'cfw:face-toolbar-button-on
                     'cfw:face-toolbar-button-off))
      (define-key keymap [mouse-1] command)
      (cfw:tp text 'keymap keymap)
      (cfw:tp text 'mouse-face 'highlight)
      text))
