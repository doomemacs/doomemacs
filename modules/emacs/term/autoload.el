;;; emacs/term/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +term/open (arg)
  "Open a terminal buffer in the current window. If ARG (universal argument) is
non-nil, cd into the current project's root."
  (interactive "P")
  (let ((default-directory
          (if arg
              (or (doom-project-root) default-directory)
            default-directory)))
    ;; Doom's switch-buffer hooks prevent themselves from triggering when
    ;; switching from buffer A back to A. Because `multi-term' uses `set-buffer'
    ;; before `switch-to-buffer', the hooks don't trigger, so we use this
    ;; roundabout way to trigger them properly.
    (switch-to-buffer (save-window-excursion (multi-term)))))

;;;###autoload
(defun +term/open-popup (arg)
  "Open a terminal popup window. If ARG (universal argument) is
non-nil, cd into the current project's root."
  (interactive "P")
  (let ((default-directory
          (if arg
              (or (doom-project-root) default-directory)
            default-directory)))
    (pop-to-buffer (save-window-excursion (multi-term)))))

;;;###autoload
(defun +term/open-popup-in-project ()
  "Open a terminal popup window in the root of the current project."
  (interactive)
  (+term/open-popup t))
