;;; core/version-control/config.el

(use-package! gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$"
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package! gitignore-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")


(use-package! browse-at-remote
  :commands (browse-at-remote/browse browse-at-remote/get-url))


(use-package! magit
  :commands magit-status
  :config
  (def-popup! ("^\\*magit.+" :regexp t))
  (after! evil-snipe
    ;; evil-snipe conflicts with magit
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)))


(use-package! evil-magit
  :when (featurep 'evil)
  :after magit)


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

