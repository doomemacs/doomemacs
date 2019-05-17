;;; tools/eval/config.el -*- lexical-binding: t; -*-

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

  (defun +eval*quickrun--outputter-replace-region ()
    "Make `quickrun-replace-region' recognize evil visual selections."
    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer quickrun--original-buffer
        (cl-destructuring-bind (beg . end)
            ;; Because `deactivate-mark', the function, was used in
            ;; `quickrun--region-command-common' instead of `deactivate-mark',
            ;; the variable, the selection is disabled by this point.
            (if (bound-and-true-p evil-local-mode)
                (cons evil-visual-beginning evil-visual-end)
              (cons (region-beginning) (region-end)))
          (delete-region beg end)
          (insert output))
        (setq quickrun-option-outputter quickrun--original-outputter))))
  (advice-add #'quickrun--outputter-replace-region :override #'+eval*quickrun--outputter-replace-region)

  (defun +eval|quickrun-shrink-window ()
    "Shrink the quickrun output window once code evaluation is complete."
    (when-let* ((win (get-buffer-window quickrun--buffer-name)))
      (with-selected-window (get-buffer-window quickrun--buffer-name)
        (let ((ignore-window-parameters t))
          (shrink-window-if-larger-than-buffer)))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-shrink-window)

  (defun +eval|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (when-let* ((win (get-buffer-window quickrun--buffer-name)))
      (with-selected-window win
        (goto-char (point-min)))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-scroll-to-bof))
