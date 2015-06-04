(use-package ido
  :defines (flx-ido-mode ido-ubiquitous-debug-mode ido-context-switch-command ido-temp-list)
  :functions (ido-to-end)
  :commands (ido-mode
             ido-everywhere
             ido-vertical-mode
             flx-ido-mode
             ido-ubiquitous-mode
             ido-find-file
             ido-find-file-in-dir)
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (use-package ido-vertical-mode  :config (ido-vertical-mode 1))
    (use-package flx-ido            :config (flx-ido-mode 1))
    (use-package ido-ubiquitous     :config (ido-ubiquitous-mode 1))

    (setq ido-use-faces nil
          ido-confirm-unique-completion t
          ido-case-fold t
          ido-enable-tramp-completion nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-enable-tramp-completion t
          ido-enable-last-directory-history t
          ido-save-directory-list-file (concat TMP-DIR "ido.last"))

    (add-to-list 'ido-ignore-files "\\`.DS_Store$")
    (add-to-list 'ido-ignore-files "Icon\\?$")
    (setq ido-ignore-buffers
          '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
            "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
            "_region_" " output\\*$" "^TAGS$" "^\*Ido"))

                                        ; sort ido filelist by mtime instead of alphabetically
    (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
    (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
    (defun ido-sort-mtime ()
      (setq ido-temp-list
            (sort ido-temp-list
                  (lambda (a b)
                    (time-less-p
                     (sixth (file-attributes (concat ido-current-directory b)))
                     (sixth (file-attributes (concat ido-current-directory a)))))))
      (ido-to-end  ;; move . files to end (again)
       (delq nil (mapcar
                  (lambda (x) (and (char-equal (string-to-char x) ?.) x))
                  ido-temp-list))))

    ;; Press ~ to go to $HOME in ido
    (add-hook! 'ido-setup-hook
                ;; Go straight home
                (define-key ido-file-completion-map (kbd "~")
                  (λ (if (looking-back "/")
                         (insert "~/")
                       (call-interactively 'self-insert-command)))))))


(provide 'init-ido)
;;; init-ido.el ends here
