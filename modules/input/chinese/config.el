;;; input/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pyim
  :after-call after-find-file pre-command-hook
  :init
  (setq pyim-dcache-directory (concat doom-cache-dir "pyim/"))
  :config
  (setq pyim-page-tooltip t
        default-input-method "pyim")

  (after! evil-escape
    (defun +input-method-activate-p ()
      current-input-method)
    (add-to-list 'evil-escape-inhibit-functions #'+input-method-activate-p))

  (when (modulep! +childframe)
    (setq pyim-page-tooltip 'posframe))

  ;; allow vertico/selectrum search with pinyin
  (cond ((modulep! :completion vertico)
         (defadvice! +pinyin-orderless-regexp (result)
           :filter-return 'orderless-regexp
           (let ((regexp-build-func (if (modulep! :editor evil +everywhere)
                                        #'evil-pinyin--build-regexp-string
                                      #'pyim-cregexp-build-regexp-string)))
             (funcall regexp-build-func result))))
        ((modulep! :completion ivy)
         (setq ivy-re-builders-alist
               '((t . pyim-cregexp-ivy))))))


(use-package! liberime
  :when (modulep! +rime)
  :init
  (setq liberime-auto-build t)
  (setq liberime-user-data-dir (expand-file-name "rime" doom-cache-dir)))


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
