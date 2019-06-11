;;; term/term/autoload.el -*- lexical-binding: t; -*-

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
        confirm-kill-processes
        current-prefix-arg)
    (cl-letf (((symbol-function #'multi-term-dedicated-get-window)
               (lambda () (setq multi-term-dedicated-window
                           (display-buffer-in-side-window
                            multi-term-dedicated-buffer
                            `((window-height . ,multi-term-dedicated-window-height)))))))
      (when arg
        (when (multi-term-window-exist-p multi-term-dedicated-window)
          (delete-window multi-term-dedicated-window))
        (when (multi-term-buffer-exist-p multi-term-dedicated-buffer)
          (when-let (process (get-buffer-process multi-term-dedicated-buffer))
            (kill-process process))
          (kill-buffer multi-term-dedicated-buffer)))
      (if (multi-term-dedicated-exist-p)
          (if (eq (selected-window) multi-term-dedicated-window)
              (multi-term-dedicated-close)
            (select-window multi-term-dedicated-window))
        (multi-term-dedicated-open)))))

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
