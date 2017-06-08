;;; app/twitter/config.el -*- lexical-binding: t; -*-

(def-package! twittering-mode
  :commands twit
  :config
  (setq twittering-use-master-password t
        twittering-icon-mode nil
        ;; twittering-use-icon-storage t
        ;; twittering-icon-storage-file (concat doom-cache-dir "twittering-mode-icons.gz")
        ;; twittering-convert-fix-size 12
        twittering-timeline-header ""
        twittering-timeline-footer ""
        twittering-edit-skeleton 'inherit-any
        twittering-status-format
        "%RT{%FACE[bold]{RT }}%S (%FACE[bold]{@%s}), %@%r%R:\n%FOLD[   ]{%t %QT{\n+----\n%FOLD[|]{ %S (@%s), %@:\n%FOLD[   ]{%t}}\n+----}}\n "
        twittering-initial-timeline-spec-string
        '(":home" ":mentions" ":direct_messages"))

  (set! :popup "*twittering-edit*" :size 12 :select t)

  (add-hook! twittering-mode
    (setq header-line-format (or (doom-modeline 'twitter) mode-line-format)
          mode-line-format nil))

  (map! :map twittering-mode-map
        [remap twittering-kill-buffer] #'+twitter/quit
        "Q" #'+twitter/quit-all
        "o" #'ace-link-addr
        "j" #'evil-next-visual-line
        "k" #'evil-previous-visual-line
        "J" #'twittering-goto-next-status
        "K" #'twittering-goto-previous-status)

  (def-modeline! twitter
    (bar matches " %b " selection-info)
    ()))
