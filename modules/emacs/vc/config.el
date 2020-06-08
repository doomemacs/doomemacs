;;; emacs/vc/config.el -*- lexical-binding: t; -*-

(when IS-WINDOWS
  (setenv "GIT_ASKPASS" "git-gui--askpass"))


(after! log-view
  (set-evil-initial-state!
    '(log-view-mode
      vc-git-log-view-mode
      vc-hg-log-view-mode
      vc-bzr-log-view-mode
      vc-svn-log-view-mode)
    'emacs)
  (map! :map log-view-mode-map
        "j" #'log-view-msg-next
        "k" #'log-view-msg-prev))


(after! vc-annotate
  (set-popup-rules!
    '(("^\\*vc-diff" :select nil)   ; *vc-diff*
      ("^\\*vc-change" :select t))) ; *vc-change-log*
  (set-evil-initial-state! 'vc-annotate-mode 'normal)

  ;; Clean up after itself
  (define-key vc-annotate-mode-map [remap quit-window] #'kill-current-buffer))


(after! vc-dir
  (set-evil-initial-state! 'vc-dir-mode 'emacs))


(after! git-timemachine
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t)

  (defadvice! +vc-update-header-line-a (revision)
    "Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting revision
info in the `header-line-format' is a good indication."
    :override #'git-timemachine--show-minibuffer-details
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize author 'face 'git-timemachine-minibuffer-author-face)
                    (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                    date-full date-relative))))

  (after! evil
    ;; rehash evil keybindings so they are recognized
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

  (when (featurep! :tools magit)
    (add-transient-hook! #'git-timemachine-blame (require 'magit-blame)))

  (map! :map git-timemachine-mode-map
        :n "C-p" #'git-timemachine-show-previous-revision
        :n "C-n" #'git-timemachine-show-next-revision
        :n "gb"  #'git-timemachine-blame
        :n "gtc" #'git-timemachine-show-commit))


(use-package! git-commit
  :hook (doom-first-file . global-git-commit-mode)
  :config
  (set-yas-minor-mode! 'git-commit-mode)

  ;; Enforce git commit conventions.
  ;; See https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (setq-hook! 'git-commit-mode-hook fill-column 72)

  (add-hook! 'git-commit-setup-hook
    (defun +vc-start-in-insert-state-maybe-h ()
      "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
      (when (and (bound-and-true-p evil-mode)
                 (bobp) (eolp))
        (evil-insert-state)))))


(after! browse-at-remote
  (setq browse-at-remote-add-line-number-if-no-region-selected nil)

  ;; HACK `browse-at-remote' produces urls with `nil' in them, when the repo is
  ;;      detached. This creates broken links. I think it is more sensible to
  ;;      fall back to master in those cases.
  (defadvice! +vc--fallback-to-master-branch-a ()
    "Return 'master' in detached state."
    :after-until #'browse-at-remote--get-local-branch
    "master"))
