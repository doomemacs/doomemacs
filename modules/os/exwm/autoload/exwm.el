;;; private/exwm/autoload/exwm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +exwm-do-mouse-click (x y &optional button-num window-id)
  "Perform a mouse click at (window relative) position X and Y

By default BUTTON-NUM is ``1'' (i.e. main click) and the WINDOW-ID is the currently selected window.

This function was taken from:
https://github.com/ch11ng/exwm/issues/693#issuecomment-750928572"
  (let* ((button-index (intern (format "xcb:ButtonIndex:%d" (or button-num 1))))
         (button-mask (intern (format "xcb:ButtonMask:%d" (or button-num 1))))
         (window-id (or window-id (exwm--buffer->id
                                   (window-buffer (selected-window)))
                        (user-error "No window selected")))
         (button-actions `((xcb:ButtonPress . ,button-mask)
                           (xcb:ButtonRelease . 0))))
    (dolist (b-action button-actions)
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination window-id
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal
                                 (make-instance (car b-action)
                                                :detail button-index
                                                :time xcb:Time:CurrentTime
                                                :root exwm--root
                                                :event window-id
                                                :child 0
                                                :root-x 0
                                                :root-y 0
                                                :event-x x
                                                :event-y y
                                                :state (cdr b-action)
                                                :same-screen 0)
                                 exwm--connection))))
    (xcb:flush exwm--connection)))

;;;###autoload
(defun +exwm-refocus-application (&rest _)
  "Click on a specific portion of the application to refocus input."
  (run-at-time
   0.02 nil
   (defun +exwm-refocus-application--timer (&rest _)
     (cl-destructuring-bind
         ((mouse-x . mouse-y) dim-x dim-y)
         (list (mouse-absolute-pixel-position)
               (window-pixel-width) (window-pixel-height))
       (when (exwm--buffer->id (current-buffer))
         ;; All that matters is that this spot be blank in the application.
         (+exwm-do-mouse-click (floor (* dim-x (/ 1.0 1000)))
                               (floor (* dim-y (/ 120.0 1410)))))))))

;;;###autoload
(defun +exwm-rename-buffer-to-title ()
  "Rename the buffer to its `exwm-title'."
  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
              (string= "gimp" exwm-instance-name))
    (exwm-workspace-rename-buffer exwm-title)))

;;;###autoload
(defun exwm--update-utf8-title-advice (oldfun id &optional force)
  "Only update the window title when the buffer is visible."
  (when (get-buffer-window (exwm--id->buffer id))
    (funcall oldfun id force)))
