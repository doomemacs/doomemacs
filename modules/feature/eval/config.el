;;; feature/eval/config.el -*- lexical-binding: t; -*-

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)


;;
;; Packages

(after! quickrun
  (setq quickrun-focus-p nil)

  (set-popup-rule! "^\\*quickrun" :size 0.3 :ttl 0)

  (defun +eval*quickrun-auto-close (&rest _)
    "Allows us to silently re-run quickrun from within the quickrun buffer."
    (when-let* ((win (get-buffer-window quickrun--buffer-name)))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))
  (advice-add #'quickrun :before #'+eval*quickrun-auto-close)
  (advice-add #'quickrun-region :before #'+eval*quickrun-auto-close)

  (defun +eval|quickrun-shrink-window ()
    "Shrink the quickrun output window once code evaluation is complete."
    (with-selected-window (get-buffer-window quickrun--buffer-name)
      (let ((ignore-window-parameters t))
        (shrink-window-if-larger-than-buffer))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-shrink-window)

  (defun +eval|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (with-selected-window (get-buffer-window quickrun--buffer-name)
      (goto-char (point-min))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-scroll-to-bof))

