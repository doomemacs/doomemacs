;;; module-crystal.el

(use-package crystal-mode
  :mode "\\.cr$"
  :interpreter "crystal"
  :init
  (after! editorconfig
    (push '(crystal-mode crystal-indent-level)
          editorconfig-indentation-alist))
  :config
  (after! quickrun
    (quickrun-add-command
     "crystal" '((:command . "crystal")
                 (:exec . "%c %s")
                 (:description . "Run Crystal script"))
     :mode 'crystal-mode)))

(provide 'module-crystal)
;;; module-crystal.el ends here
