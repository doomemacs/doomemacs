;;; emacs/vterm/config.el -*- lexical-binding: t; -*-

(def-package! vterm
  :load-path (lambda () (list (concat doom-packages-dir "/quelpa/build/vterm")))
  :init
  (unless (file-executable-p (concat
                              (file-name-directory (locate-library "vterm"))
                              "vterm-module.so"))
    ;; let vterm compile `vterm-modules.so'
    (setq-default vterm-install t))
  :when (and (string-match-p "MODULES" system-configuration-features)
             (display-graphic-p))
  :config
  (set-env! "SHELL")
  (add-hook 'vterm-mode-hook #'doom|mark-buffer-as-real)
  ;; Automatically kill buffer when vterm exits.
  (add-hook 'vterm-exit-functions #'(lambda (buffer)
                                      (when buffer (kill-buffer buffer))))
  (when (featurep! :feature evil)
    (evil-set-initial-state 'vterm-mode 'insert)
    ;; Those keys are commonly needed by terminals.
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-d") #'vterm--self-insert)
    ;; Go back to normal state but don't move cursor backwards.
    ;; Moving cursor backwards is the default Vim behavior but
    ;; it is not appropriate in some cases like terminals.
    (add-hook 'vterm-mode-hook #'(lambda ()
                                   (setq-local evil-move-cursor-back nil)))))
