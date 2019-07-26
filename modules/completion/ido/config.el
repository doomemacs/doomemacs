;;; completion/ido/config.el -*- lexical-binding: t; -*-

(defun +ido-init-h ()
  (setq ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
          "_region_" " output\\*$" "^TAGS$" "^\*Ido")
        ido-use-faces nil
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

  (define-key! (ido-common-completion-map ido-completion-map ido-file-completion-map)
    "\C-n" #'ido-next-match
    "\C-p" #'ido-prev-match
    "\C-w" #'ido-delete-backward-word-updir
    ;; Go to $HOME with ~
    "~" (λ! (if (looking-back "/" (point-min))
                (insert "~/")
              (call-interactively #'self-insert-command))))

  (defadvice! +ido--sort-mtime-a ()
    "Sort ido filelist by mtime instead of alphabetically."
    :override #'ido-sort-mtime
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a)))))))
    (ido-to-end  ;; move . files to end (again)
     (cl-loop for x in ido-temp-list
              if (char-equal (string-to-char x) ?.)
              collect x)))
  (add-hook! '(ido-make-file-list-hook ido-make-dir-list-hook)
             #'ido-sort-mtime)

  ;;
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)
  (flx-ido-mode +1)
  (crm-custom-mode +1)

  ;;
  (remove-hook 'ido-setup-hook #'+ido-init-h))

;;
(add-hook 'ido-setup-hook #'+ido-init-h)
