;;; app/twitter/config.el -*- lexical-binding: t; -*-

(use-package! twittering-mode
  :commands twit
  :config
  (setq twittering-private-info-file
        (expand-file-name "twittering-mode.gpg" doom-data-dir)
        twittering-use-master-password t
        twittering-request-confirmation-on-posting t
        ;; twittering-icon-mode t
        ;; twittering-use-icon-storage t
        ;; twittering-icon-storage-file (concat doom-cache-dir "twittering-mode-icons.gz")
        ;; twittering-convert-fix-size 12
        twittering-timeline-header ""
        twittering-timeline-footer ""
        twittering-edit-skeleton 'inherit-any
        twittering-status-format "%FACE[font-lock-function-name-face]{  @%s}  %FACE[italic]{%@}  %FACE[error]{%FIELD-IF-NONZERO[❤ %d]{favorite_count}}  %FACE[warning]{%FIELD-IF-NONZERO[↺ %d]{retweet_count}}
%FOLD[   ]{%FILL{%t}%QT{
%FOLD[   ]{%FACE[font-lock-function-name-face]{@%s}\t%FACE[shadow]{%@}
%FOLD[ ]{%FILL{%t}}
}}}

%FACE[twitter-divider]{                                                                                                }
"
        ;; twittering-timeline-spec-alias '()
        twittering-initial-timeline-spec-string
        '(":home" ":mentions" ":direct_messages"))

  (set-popup-rule! "^\\*twittering-edit" :size 15 :ttl nil :quit nil :select t)

  (defface twitter-divider
    '((((background dark))  (:underline (:color "#141519")))
      (((background light)) (:underline (:color "#d3d3d3"))))
    "The vertical divider between tweets."
    :group 'twittering-mode)

  (add-hook 'doom-real-buffer-functions #'+twitter-buffer-p)
  (when (modulep! :ui popup)
    (setq twittering-pop-to-buffer-function #'+twitter-display-buffer-fn))

  ;; Custom header-line for twitter buffers
  (add-hook! 'twittering-mode-hook
    (defun +twitter-switch-mode-and-header-line-h ()
      (setq header-line-format mode-line-format
            mode-line-format nil)))

  ;; `epa--decode-coding-string' isn't defined in later versions of Emacs 27
  (unless (fboundp 'epa--decode-coding-string)
    (defalias 'epa--decode-coding-string #'decode-coding-string))

  (define-key! twittering-mode-map
    "q" #'+twitter/quit
    "Q" #'+twitter/quit-all
    [remap twittering-kill-buffer] #'+twitter/quit
    [remap delete-window]          #'+twitter/quit
    [remap +workspace/close-window-or-workspace] #'+twitter/quit)
  (when (modulep! :editor evil +everywhere)
    (define-key! twittering-mode-map
      [remap evil-window-delete] #'+twitter/quit
      "f"    #'twittering-favorite
      "F"    #'twittering-unfavorite
      "\C-f" #'twittering-follow
      "\C-F" #'twittering-unfollow
      "d"    #'twittering-delete-status
      "r"    #'twittering-retweet
      "R"    #'twittering-toggle-or-retrieve-replied-statuses
      "o"    #'twittering-update-status-interactive
      "O"    #'+twitter/ace-link
      "/"    #'twittering-search
      "J"    #'twittering-goto-next-status
      "K"    #'twittering-goto-previous-status
      "g"    nil
      "gg"   #'twittering-goto-first-status
      "G"    #'twittering-goto-last-status
      "gj"   #'twittering-goto-next-status-of-user
      "gk"   #'twittering-goto-previous-status-of-user)))
