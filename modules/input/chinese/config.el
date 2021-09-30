;;; input/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pyim
  :after-call after-find-file pre-command-hook
  :init
  (setq pyim-dcache-directory (concat doom-cache-dir "pyim/"))
  :config
  (setq pyim-page-tooltip t
        default-input-method "pyim")


  ;; allow vertico/selectrum search with pinyin
  (cond ((featurep! :completion vertico)
         (defadvice! +pinyin-orderless-regexp (orig-fn component)
           :around 'orderless-regexp
           (let ((result (funcall orig-fn component)))
             (pyim-cregexp-build result))))
        ((featurep! :completion ivy)
         (setq ivy-re-builders-alist
               '((t . pyim-cregexp-ivy))))))

(use-package! liberime
  :when (featurep! +rime)
  :init
  (setq liberime-auto-build t)
  (setq liberime-user-data-dir (expand-file-name "rime" doom-cache-dir)))

(use-package! pyim-liberime
  :when (featurep! +rime)
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
