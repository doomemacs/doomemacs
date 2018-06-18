;;; feature/eval/config.el -*- lexical-binding: t; -*-

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)


;;
;; Plugin(s)
;;

(def-package! quickrun
  :defer t
  :init
  (unless (boundp 'display-line-numbers)
    (add-hook 'quickrun--mode-hook #'nlinum-mode))
  :config
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

  (defun +eval|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (with-selected-window (get-buffer-window quickrun--buffer-name)
      (goto-char (point-min))
      (let ((ignore-window-parameters t))
        (shrink-window-if-larger-than-buffer))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-scroll-to-bof))

