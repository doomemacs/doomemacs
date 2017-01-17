;;; core-vcs.el

(package! gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$"
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(package! gitignore-mode
  :mode "/\\.gitignore$"
  :mode "/\\.git/info/exclude$"
  :mode "/git/ignore$")

(package! git-gutter-fringe
  :commands git-gutter-mode
  :init (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
  :config
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)

  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)

  ;; Refreshing git-gutter on ESC and focus
  (advice-add 'evil-force-normal-state :after 'git-gutter)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows))

(package! magit
  :commands magit-status
  :config
  ;; Prevent magit + evil-snipe conflicts
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

  (map! :map magit-mode-map
        ;; Don't let Tab binding in my-bindings conflict with Tab in magit
        :m "<tab>" 'magit-section-toggle
        ;; Don't interfere with window movement keys
        :nv "C-j" nil
        :nv "C-k" nil))

(package! evil-magit :after magit)

(package! browse-at-remote
  :commands (browse-at-remote/browse browse-at-remote/get-url))

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


;;
;; Defuns
;;

(defun doom-git-root ()
  "Get git url root."
  (when-let (url (car-safe (browse-at-remote--remote-ref buffer-file-name)))
    (cdr (browse-at-remote--get-url-from-remote url))))

(defun doom/git-browse-issues ()
  "Open the github issues page for current repo."
  (interactive)
  (if-let (root (doom-git-root))
      (browse-url (concat root "/issues"))
    (user-error "No git root found!")))

(evil-define-command doom:git-browse (&optional bang)
  "Open the website for the current (or specified) version controlled FILE. If
BANG, then copy it to clipboard. Fallback to repository root."
  (interactive "<!>")
  (let (url)
    (condition-case err
        (setq url (browse-at-remote-get-url))
      (error
       (setq url (shell-command-to-string "hub browse -u --"))
       (setq url (if url
                     (concat (s-trim url) "/" (f-relative (buffer-file-name) (doom-project-root))
                             (when (use-region-p) (format "#L%s-L%s"
                                                          (line-number-at-pos (region-beginning))
                                                          (line-number-at-pos (region-end)))))))))
    (when url
      (if bang
          (message "Url copied to clipboard: %s" (kill-new url))
        (browse-url url)))))

(provide 'core-vcs)
;;; core-vcs.el ends here
