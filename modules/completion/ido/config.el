;;; completion/ido/config.el -*- lexical-binding: t; -*-

(def-package! ido
  :config
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

  (push "\\`.DS_Store$" ido-ignore-files)
  (push "Icon\\?$" ido-ignore-files)

  (ido-mode 1)
  (ido-everywhere 1)
  (require 'ido-ubiquitous)

  (defun +ido|init ()
    (require 'ido-vertical-mode)
    (require 'flx-ido)
    (require 'crm-custom)
    (map! :map (ido-common-completion-map ido-completion-map ido-file-completion-map)
          "C-n" #'ido-next-match
          "C-p" #'ido-prev-match
          "C-w" #'ido-delete-backward-word-updir))
  (add-hook 'ido-setup-hook #'+ido|init)

  (defun +ido*sort-mtime ()
    "Sort ido filelist by mtime instead of alphabetically."
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
  (advice-add #'ido-sort-mtime :override #'+ido*sort-mtime)
  (add-hook! (ido-make-file-list ido-make-dir-list) #'+ido*sort-mtime)

  (defun +ido|setup-home-keybind ()
    "Go to $HOME with ~"
    (define-key ido-file-completion-map (kbd "~")
      (λ! (if (looking-back "/" (point-min))
             (insert "~/")
           (call-interactively #'self-insert-command)))))
  (add-hook 'ido-setup-hook #'+ido|setup-home-keybind))


(def-package! ido-ubiquitous :config (ido-ubiquitous-mode 1))

(def-package! ido-vertical-mode :config (ido-vertical-mode 1))

(def-package! flx-ido :config (flx-ido-mode +1))

(def-package! crm-custom :config (crm-custom-mode +1))
