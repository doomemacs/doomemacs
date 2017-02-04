;;; core/version-control/config.el

(load! +git)
;; (load! +hg)

(after! vc-annotate
  (set! :popup
    ("*vc-diff*" :size 15 :noselect t)
    ("*vc-change-log*" :size 15 :select t)
    (vc-annotate-mode :same t))

  (set! :evil-state
    (vc-annotate-mode normal)
    (vc-git-log-view-mode normal))

  (map! :map vc-annotate-mode-map
        :n "q" 'kill-this-buffer
        :n "d" 'vc-annotate-show-diff-revision-at-line
        :n "D" 'vc-annotate-show-changeset-diff-revision-at-line
        :n "SPC" 'vc-annotate-show-log-revision-at-line
        :n "]]" 'vc-annotate-next-revision
        :n "[[" 'vc-annotate-prev-revision
        :n [tab] 'vc-annotate-toggle-annotation-visibility
        :n "RET" 'vc-annotate-find-revision-at-line))

