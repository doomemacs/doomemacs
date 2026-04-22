;;; term/vterm/config.el -*- lexical-binding: t; -*-

(use-package! vterm
  :when (bound-and-true-p module-file-suffix)  ; requires dynamic-modules support
  :commands vterm-mode
  :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :preface
  ;; HACK: Because vterm clusmily forces vterm-module.so's compilation on us
  ;;   when the package is loaded, this is necessary to prevent it when
  ;;   byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;   when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))

  :config
  (set-popup-rule! "^\\*vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  (map! :map vterm-mode-map "C-q" #'vterm-send-next-key
        :n "0"  (cmd! (vterm-send-key "a" nil nil t)
                      (evil-refresh-cursor))

        :n "dd" (cmd! (vterm-send-key "e" nil nil t)
                      (vterm-send-key "u" nil nil t)
                      (evil-refresh-cursor))

        :n "V"  (cmd! (let* ((beg (save-excursion
                                    (beginning-of-line)
                                    (re-search-forward "\\$ " (line-end-position) t)
                                    (point)))
                             (end (save-excursion (vterm-end-of-line) (point))))
                        (evil-visual-make-region beg end))))


  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)

  (setq-hook! 'vterm-mode-hook
    ;; Don't prompt about dying processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0))
