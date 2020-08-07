;;; term/vterm/config.el -*- lexical-binding: t; -*-

(use-package! vterm
  :when (bound-and-true-p module-file-suffix)
  :commands vterm vterm-mode
  :preface (setq vterm-install t) ; compile the package when you load vterm
  :hook (vterm-mode . doom-mark-buffer-as-real-h)
  :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :preface
  (when! (fboundp 'native-compile)
    ;; HACK Fix #3683: `vterm-module-compile' uses `locate-library' to determine
    ;;      where vterm-module.so should be, but an eln file (for emacsGcc
    ;;      users) reports a subdirectory that will never contain it; rendering
    ;;      vterm unable to build itself.
    ;; REVIEW Remove this when akermu/emacs-libvterm#363 is resolved.
    (defadvice! +vterm--dont-resolve-to-eln-file-a (orig-fn &rest args)
      :around #'vterm-module-compile
      (let ((load-suffixes (remove ".eln" load-suffixes)))
        (apply orig-fn args))))

  :config
  (set-popup-rule! "^vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)

  (setq-hook! 'vterm-mode-hook
    ;; Don't prompt about dying processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0)

  ;; Restore the point's location when leaving and re-entering insert mode.
  (when (featurep! :editor evil)
    (add-hook! 'vterm-mode-hook
      (defun +vterm-init-remember-point-h ()
        (add-hook 'evil-insert-state-exit-hook #'+vterm-remember-insert-point-h nil t)
        (add-hook 'evil-insert-state-entry-hook #'+vterm-goto-insert-point-h nil t)))))
