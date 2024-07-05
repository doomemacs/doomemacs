;;; app/calendar/autoload.el -*- lexical-binding: t; -*-

(defvar +calendar--wconf nil)
(defvar +calendar-workspace-name "*calendar*"
  "Name of the workspace created by `=calendar', dedicated to calfw.")

(defun +calendar--init ()
  (if-let (win (cl-find-if (lambda (b) (string-match-p "^\\*cfw:" (buffer-name b)))
                           (doom-visible-windows)
                           :key #'window-buffer))
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
                      '(cfw:details-mode
                        cfw:calendar-mode))
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
(defun +calendar/open-calendar ()
  "TODO"
  (interactive)
  (cfw:open-calendar-buffer
   ;; :custom-map cfw:my-cal-map
   :contents-sources
   (list
    (cfw:org-create-source (face-foreground 'default))  ; orgmode source
    )))

;;;###autoload
(defun +calendar-cfw:render-button-a (title command &optional state)
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
