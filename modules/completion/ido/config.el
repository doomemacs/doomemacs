;;; completion/ido/config.el -*- lexical-binding: t; -*-

(defvar ido-mode-hook nil
  "List of hooks to run when `ido-mode' is activated.")


;;
;;; Packages

(use-package! ido
  :hook (doom-first-input . ido-mode)
  :hook (ido-mode . ido-everywhere)
  :hook (ido-mode . ido-ubiquitous-mode)
  :preface
  ;; HACK `ido' is a really old package. It defines `ido-mode' manually and
  ;;      doesn't define a hook, so we define one for it, so we can use it!
  (defadvice! +ido-run-hooks-a (&rest _)
    :after #'ido-mode
    (run-hooks 'ido-mode-hook))
  :init
  (setq ido-save-directory-list-file (concat doom-cache-dir "ido.last"))
  :config
  (pushnew! ido-ignore-files "\\`.DS_Store$" "Icon\\?$")
  (setq ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*[Hh]elp" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-" "_region_"
          " output\\*$" "^TAGS$" "^\*Ido")
        ido-auto-merge-work-directories-length -1
        ido-confirm-unique-completion t
        ido-case-fold t
        ido-create-new-buffer 'always
        ido-enable-flex-matching t)

  (map! :map (ido-common-completion-map ido-file-completion-map)
        "C-w"  #'ido-delete-backward-word-updir
        :map (ido-common-completion-map ido-file-dir-completion-map)
        "C-n"  #'ido-next-match
        "C-p"  #'ido-prev-match
        [down] #'ido-next-match
        [up]   #'ido-prev-match
        :map ido-file-completion-map
        ;; Go to $HOME with ~
        "~"    (cmd! (if (looking-back "/" (point-min))
                         (insert "~/")
                       (call-interactively #'self-insert-command)))))


(use-package! ido-vertical-mode
  :hook (ido-mode . ido-vertical-mode)
  :config (setq ido-vertical-show-count t))


(use-package! ido-sort-mtime
  :hook (ido-mode . ido-sort-mtime-mode))


(use-package! crm-custom
  :hook (ido-mode . crm-custom-mode))


(use-package! flx-ido
  :hook (ido-mode . flx-ido-mode))
