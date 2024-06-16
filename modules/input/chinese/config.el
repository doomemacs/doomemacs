;;; input/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pyim
  :after-call after-find-file pre-command-hook
  :init
  (setq pyim-dcache-directory (concat doom-cache-dir "pyim/"))
  :config
  (setq pyim-page-tooltip t
        default-input-method "pyim")

  (after! evil-escape
    (defun +chinese--input-method-p ()
      current-input-method)
    (add-to-list 'evil-escape-inhibit-functions #'+chinese--input-method-p))

  (when (modulep! +childframe)
    (setq pyim-page-tooltip 'posframe))

  ;; allow vertico/selectrum search with pinyin
  (cond ((modulep! :completion vertico)
         (advice-add #'orderless-regexp
                     :filter-return
                     (if (modulep! :editor evil +everywhere)
                         #'evil-pinyin--build-regexp-string
                       #'pyim-cregexp-build)))
        ((modulep! :completion ivy)
         (autoload 'pyim-cregexp-ivy "pyim-cregexp-utils")
         (setq ivy-re-builders-alist '((t . pyim-cregexp-ivy))))))


(use-package! liberime
  :when (modulep! +rime)
  :after pyim
  :init
  (setq liberime-auto-build t
        liberime-user-data-dir (file-name-concat doom-cache-dir "rime")))


(use-package! pyim-liberime
  :when (modulep! +rime)
  :after liberime
  :config
  (setq pyim-default-scheme 'rime))


(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  ;; Always insert `real' space in org-mode.
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t))


(use-package! fcitx
  :after evil
  :config
  (when (setq fcitx-remote-command
              (or (executable-find "fcitx5-remote")
                  (executable-find "fcitx-remote")))
    (fcitx-evil-turn-on)))


(use-package! ace-pinyin
  :after avy
  :init (setq ace-pinyin-use-avy t)
  :config (ace-pinyin-global-mode t))


(use-package! evil-pinyin
  :when (modulep! :editor evil +everywhere)
  :after evil
  :config
  (setq-default evil-pinyin-with-search-rule 'always)
  (global-evil-pinyin-mode 1))


;;
;;; Hacks

(defadvice! +chinese--org-html-paragraph-a (args)
  "Join consecutive Chinese lines into a single long line without unwanted space
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
