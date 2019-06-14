;;; term/term/autoload.el -*- lexical-binding: t; -*-

(defvar +term--dedicated-buffer nil)
(defvar +term--dedicated-window nil)

;;;###autoload
(defun +term/toggle (arg)
  "Toggle a persistent terminal popup window at project's root.

If popup is visible but unselected, select it.

If prefix ARG, recreate term buffer in the current project's root."
  (interactive "P")
  (require 'multi-term)
  (let ((default-directory (or (doom-project-root) default-directory))
        (multi-term-dedicated-buffer-name "doom:term-popup")
        (multi-term-dedicated-select-after-open-p t)
        confirm-kill-processes)
    (when arg
      (when (multi-term-window-exist-p +term--dedicated-window)
        (delete-window +term--dedicated-window))
      (when (multi-term-buffer-exist-p +term--dedicated-buffer)
        (when-let (process (get-buffer-process +term--dedicated-buffer))
          (kill-process process))
        (kill-buffer +term--dedicated-buffer)))
    (if (multi-term-dedicated-exist-p)
        (if (eq (selected-window) +term--dedicated-window)
            (multi-term-dedicated-close)
          (select-window +term--dedicated-window)
          (when (bound-and-true-p evil-local-mode)
            (evil-change-to-initial-state))
          (goto-char (point-max)))
      (let ((buffer (multi-term-get-buffer nil t)))
        (with-current-buffer buffer
          (multi-term-internal)
          (setq +term--dedicated-buffer buffer))
        (when-let (window
                   (display-buffer-in-side-window
                    buffer `((window-height . ,multi-term-dedicated-window-height))))
          (setq +term--dedicated-window window)
          (select-window window))))))

;;;###autoload
(defun +term/here (arg)
  "Open a terminal buffer in the current window at project's root.

If prefix ARG is non-nil, cd into `default-directory' instead of the project
root."
  (interactive "P")
  (let ((default-directory
          (if arg
              default-directory
            (or (doom-project-root) default-directory))))
    ;; Doom's switch-buffer hooks prevent themselves from triggering when
    ;; switching from buffer A back to A. Because `multi-term' uses `set-buffer'
    ;; before `switch-to-buffer', the hooks don't trigger, so we use this
    ;; roundabout way to trigger them properly.
    (switch-to-buffer (save-window-excursion (multi-term)))))
