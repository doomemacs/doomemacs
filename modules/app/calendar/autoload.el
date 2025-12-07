;;; app/calendar/autoload.el -*- lexical-binding: t; -*-

(defvar +calendar--wconf nil)
(defvar +calendar-workspace-name "*calendar*"
  "Name of the workspace created by `=calendar', dedicated to calfw.")

(defun +calendar--init ()
  (require 'calfw)
  (if-let* ((win (get-buffer-window calfw-calendar-buffer-name)))
      (select-window win)
    (call-interactively +calendar-open-function)))

;;;###autoload
(defun =calendar ()
  "Activate (or switch to) `calendar' in its workspace."
  (interactive)
  (if (modulep! :ui workspaces)
      (progn
        (+workspace-switch +calendar-workspace-name t)
        (unless (memq (buffer-local-value 'major-mode
                                          (window-buffer (selected-window)))
                      '(calfw-details-mode
                        calfw-calendar-mode))
          (doom/switch-to-scratch-buffer)
          (+calendar--init))
        (+workspace/display))
    (setq +calendar--wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer))
    (+calendar--init)))

;;;###autoload
(defun +calendar/quit ()
  "TODO"
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +calendar-workspace-name)
        (+workspace/kill +calendar-workspace-name))
    (when (window-configuration-p +calendar--wconf)
      (set-window-configuration +calendar--wconf))
    (setq +calendar--wconf nil))
  (doom-kill-matching-buffers "^\\*cfw[:-]"))

;;;###autoload
(defun +calendar/open-calendar (&rest args)
  "TODO"
  (interactive)
  (apply #'calfw-org-open-calendar nil "org-agenda" (face-foreground 'default)
         args))

;;;###autoload
(defun +calendar-calfw-render-button-a (title command &optional state)
  "render-button
 TITLE
 COMMAND
 STATE"
  (let ((text (concat " " title " "))
        (keymap (make-sparse-keymap)))
    (calfw-rt text (if state 'calfw-face-toolbar-button-on
                   'calfw-face-toolbar-button-off))
    (define-key keymap [mouse-1] command)
    (calfw-tp text 'keymap keymap)
    (calfw-tp text 'mouse-face 'highlight)
    text))
