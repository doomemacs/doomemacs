;;; input/chinese/config.el -*- lexical-binding: t; -*-

(def-package! pyim
  :config
  (setq pyim-dcache-directory (concat doom-cache-dir "pyim/")
        pyim-page-tooltip t
        default-input-method "pyim"))


(def-package! pangu-spacing
  :hook (doom-after-init . global-pangu-spacing-mode)
  :config
  ;; Always insert `real' space in org-mode.
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t))


(def-package! fcitx
  :when (executable-find "fcitx-remote")
  :after evil
  :config (fcitx-evil-turn-on))


(def-package! ace-pinyin
  :after (:or avy ace-window)
  :config
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode t))


;;
;;; Hacks

(defun +chinese*org-html-paragraph (paragraph contents info)
  "Join consecutive Chinese lines into a single long line without unwanted space
when exporting org-mode to html."
  (let* ((fix-regexp "[[:multibyte:]]")
         (origin-contents contents)
         (fixed-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
           "\\1\\2"
           origin-contents)))
    (list paragraph fixed-contents info)))
(advice-add #'org-html-paragraph :filter-args #'+chinese*org-html-paragraph)
