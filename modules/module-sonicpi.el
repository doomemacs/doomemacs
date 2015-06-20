;;; module-sonicpi.el

(use-package sonic-pi
  :config
  (setq sonic-pi-path "/Applications/Sonic Pi.app/")
  (add-unreal-buffer! "\\*sonic-pi-server-messages\\*")
  (add-popwin-rule! "*sonic-pi-server-messages*" :position bottom :height: 0.4 :nostick t :dedicated t))

(provide 'module-sonicpi)
;;; module-sonicpi.el ends here
