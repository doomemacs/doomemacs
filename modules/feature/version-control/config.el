;;; feature/version-control/config.el

(setq vc-make-backup-files nil)

(load! +git)
;; (load! +hg)

(after! vc-annotate
  (set! :popup
    '("*vc-diff*" :size 15 :noselect t)
    '("*vc-change-log*" :size 15)
    '(vc-annotate-mode :same t))

  (set! :evil-state
    '(vc-annotate-mode . normal)
    '(vc-git-log-view-mode . normal)))
