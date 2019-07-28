;;; tools/eval/config.el -*- lexical-binding: t; -*-

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)


;;
;; Packages

(after! quickrun
  (setq quickrun-focus-p nil)

  (set-popup-rule! "^\\*quickrun" :size 0.3 :ttl 0)

  (defadvice! +eval--quickrun-auto-close-a (&rest _)
    "Allows us to silently re-run quickrun from within the quickrun buffer."
    :before '(quickrun quickrun-region)
    (when-let (win (get-buffer-window quickrun--buffer-name))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))

  (defadvice! +eval--quickrun-fix-evil-visual-region-a ()
    "Make `quickrun-replace-region' recognize evil visual selections."
    :override #'quickrun--outputter-replace-region
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

  (add-hook! 'quickrun-after-run-hook
    (defun +eval-quickrun-shrink-window-h ()
      "Shrink the quickrun output window once code evaluation is complete."
      (when-let (win (get-buffer-window quickrun--buffer-name))
        (with-selected-window (get-buffer-window quickrun--buffer-name)
          (let ((ignore-window-parameters t))
            (shrink-window-if-larger-than-buffer))))))

  (add-hook! 'quickrun-after-run-hook
    (defun +eval-quickrun-scroll-to-bof-h ()
      "Ensures window is scrolled to BOF on invocation."
      (when-let (win (get-buffer-window quickrun--buffer-name))
        (with-selected-window win
          (goto-char (point-min)))))))
