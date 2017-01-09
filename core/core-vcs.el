;;; core-vcs.el --- version control awareness

(use-package gitconfig-mode
  :mode ("/\\.?git/?config$" "/\\.gitmodules$")
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode ("/\\.gitignore$"
         "/\\.git/info/exclude$"
         "/git/ignore$"))

(use-package git-gutter
  :commands (git-gutter-mode doom/vcs-next-hunk doom/vcs-prev-hunk
             doom/vcs-show-hunk doom/vcs-stage-hunk doom/vcs-revert-hunk)
  :init
  (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
  :config
  (require 'git-gutter-fringe)
  (def-popup! "^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t)

  ;; NOTE If you want the git gutter to be on the outside of the margins (rather
  ;; than inside), `fringes-outside-margins' should be non-nil.

  ;; colored fringe "bars"
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)

  ;; Refreshing git-gutter
  (advice-add 'evil-force-normal-state :after 'git-gutter)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows)

  (defalias 'doom/vcs-next-hunk    'git-gutter:next-hunk)
  (defalias 'doom/vcs-prev-hunk    'git-gutter:previous-hunk)
  (defalias 'doom/vcs-show-hunk    'git-gutter:popup-hunk)
  (defalias 'doom/vcs-stage-hunk   'git-gutter:stage-hunk)
  (defalias 'doom/vcs-revert-hunk  'git-gutter:revert-hunk))

(use-package git-messenger
  :commands git-messenger:popup-message
  :init (defvar git-messenger-map (make-sparse-keymap))
  :config
  (def-popup! "*git-messenger*" :align left :size 55 :select t)
  (setq git-messenger:show-detail t)
  (map! :map git-messenger-map
        "<escape>" 'git-messenger:popup-close
        "q"        'git-messenger:popup-close))

(use-package magit
  :commands (magit-status)
  :config
  (def-popup! "^\\*magit.+" :align below :regexp t)
  ;; Prevent magit + evil-snipe conflicts
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (require 'evil-magit)

  (setq magit-display-file-buffer-function 'doom/magit-pop-to-buffer)

  (map! :map magit-mode-map
        ;; Don't let Tab binding in my-bindings conflict with Tab in magit
        :m "<tab>" 'magit-section-toggle
        ;; Don't interfere with window movement keys
        :nv "C-j" nil
        :nv "C-k" nil))

(after! vc-annotate
  (evil-set-initial-state 'vc-annotate-mode     'normal)
  (evil-set-initial-state 'vc-git-log-view-mode 'normal)
  (map! :map vc-annotate-mode-map
        :n "q" 'kill-this-buffer
        :n "d" 'vc-annotate-show-diff-revision-at-line
        :n "D" 'vc-annotate-show-changeset-diff-revision-at-line
        :n "SPC" 'vc-annotate-show-log-revision-at-line
        :n "]]" 'vc-annotate-next-revision
        :n "[[" 'vc-annotate-prev-revision
        :n [tab] 'vc-annotate-toggle-annotation-visibility
        :n "RET" 'vc-annotate-find-revision-at-line))

(use-package browse-at-remote
  :commands (browse-at-remote/browse browse-at-remote/get-url))

;; Ediff
(defvar doom-ediff-enabled nil)
(add-hook! ediff-load
  (setq ediff-diff-options           "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain) ; no extra frames

  ;; Brighten other buffers
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)

  ;; TODO Custom modeline for ediff buffers

  ;; For modeline awareness
  (add-hook! ediff-startup (setq doom-ediff-enabled t))
  (add-hook! ediff-quit    (setq doom-ediff-enabled nil)))

(provide 'core-vcs)
;;; core-vcs.el ends here
