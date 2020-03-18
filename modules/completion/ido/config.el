;;; completion/ido/config.el -*- lexical-binding: t; -*-

(use-package! ido
  :after-call pre-command-hook
  :config
  (setq ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
          "_region_" " output\\*$" "^TAGS$" "^\*Ido")
        ido-auto-merge-work-directories-length -1
        ido-confirm-unique-completion t
        ido-case-fold t
        ido-enable-tramp-completion nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-enable-tramp-completion t
        ido-enable-last-directory-history t
        ido-save-directory-list-file (concat doom-cache-dir "ido.last"))

  (unless (member "\\`.DS_Store$" ido-ignore-files)
    (push "\\`.DS_Store$" ido-ignore-files)
    (push "Icon\\?$" ido-ignore-files))

  (defun +ido-init-h ()
    (define-key! (ido-common-completion-map ido-completion-map ido-file-completion-map)
      "C-n"    #'ido-next-match
      "C-p"    #'ido-prev-match
      "<down>" #'ido-next-match
      "<up>"   #'ido-prev-match
      "TAB"    #'ido-exit-minibuffer
      "C-w"    #'ido-delete-backward-word-updir
      ;; Go to $HOME with ~
      "~" (λ! (if (looking-back "/" (point-min))
                  (insert "~/")
                (call-interactively #'self-insert-command)))))

  ;;
  (add-transient-hook! 'ido-setup-hook (+ido-init-h))

  (ido-mode 1)
  (ido-everywhere 1))

(after! ido
  (setq ido-vertical-show-count t)

  (crm-custom-mode +1)
  (ido-ubiquitous-mode 1)
  (ido-sort-mtime-mode 1)
  (ido-vertical-mode 1)
  (flx-ido-mode +1))
