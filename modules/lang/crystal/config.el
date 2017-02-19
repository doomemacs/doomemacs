;;; module-crystal.el

(@def-package crystal-mode
  :mode "\\.cr$"
  :interpreter "crystal"
  :config
  (@set :eval 'crystal-mode
        '((:command     . "crystal")
          (:exec        . "%c %s")
          (:description . "Run Crystal script"))))

