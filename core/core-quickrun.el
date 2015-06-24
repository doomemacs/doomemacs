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
  (add-popwin-rule! "*quickrun*" :position bottom :height 15)
  (add-unreal-buffer! "\\`\\*quickrun\\*\\'"))

(provide 'core-quickrun)
;;; core-quickrun.el ends here
