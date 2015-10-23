;;; core-quickrun.el

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region
             helm-quickrun)
  :config
  (setq quickrun-focus-p nil)
  (add-to-list 'quickrun-file-alist '("\\.gvy$" . "groovy"))

  (defun narf*quickrun-close-popwin ()
    (when (get-buffer quickrun/buffer-name)
      (quickrun/kill-quickrun-buffer)
      (popwin:close-popup-window-if-necessary)))
  (advice-add 'quickrun :before 'narf*quickrun-close-popwin)
  (advice-add 'quickrun-region :before 'narf*quickrun-close-popwin))

(provide 'core-quickrun)
;;; core-quickrun.el ends here
