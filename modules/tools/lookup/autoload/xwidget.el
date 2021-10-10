;;; tools/lookup/autoload/xwidget.el -*- lexical-binding: t; -*-

(defvar +lookup--xwidget-webkit-last-session-buffer nil)

;;;###autoload
(defun +lookup-xwidget-webkit-open-url-fn (url &optional new-session)
  (if (not (display-graphic-p))
      (browse-url url)
    (unless (featurep 'xwidget-internal)
      (user-error "Your build of Emacs lacks Xwidgets support and cannot open Xwidget WebKit browser"))
    (let ((orig-last-session-buffer (if (boundp 'xwidget-webkit-last-session-buffer)
                                        xwidget-webkit-last-session-buffer
                                      nil)))
      (setq xwidget-webkit-last-session-buffer +lookup--xwidget-webkit-last-session-buffer)
      (save-window-excursion
        (xwidget-webkit-browse-url url new-session))
      (pop-to-buffer xwidget-webkit-last-session-buffer)
      (setq +lookup--xwidget-webkit-last-session-buffer xwidget-webkit-last-session-buffer
            xwidget-webkit-last-session-buffer orig-last-session-buffer))))
