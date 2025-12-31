;;; input/japanese/config.el -*- lexical-binding: t; -*-

(use-package! migemo
  :after-call after-find-file pre-command-hook
  :init
  (setq search-default-regexp-mode nil
        migemo-options '("-q" "--emacs" "-i" "\a")
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix
        migemo-directory (file-name-concat doom-profile-data-dir "migemo/")
        migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  :config
  (when (executable-find migemo-command)
    (migemo-init)

    (use-package! avy-migemo
      :after avy
      :config (avy-migemo-mode 1))

    (when (modulep! :completion helm)
      (after! helm (helm-migemo-mode +1)))))


(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :init
  ;; replacing `chinese-two-byte' by `japanese'
  (setq pangu-spacing-chinese-before-english-regexp
        "\\(?1:\\cj\\)\\(?2:[0-9A-Za-z]\\)"
        pangu-spacing-chinese-after-english-regexp
        "\\(?1:[0-9A-Za-z]\\)\\(?2:\\cj\\)"
        ;; Always insert `real' space in text-mode including org-mode.
        pangu-spacing-real-insert-separtor t))


(use-package! skk
  :general ("C-x j" #'skk-mode)
  :config
  (add-hook 'doom-escape-hook #'skk-mode-exit))


;;
;;; Hacks

(defadvice! +japanese--org-html-paragraph-a (args)
  "Join consecutive Japanese lines into a single long line without unwanted space
when exporting org-mode to html."
  :filter-args #'org-html-paragraph
  (cl-destructuring-bind (paragraph contents info) args
    (let* ((fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
             "\\1\\2"
             contents)))
      (list paragraph fixed-contents info))))
