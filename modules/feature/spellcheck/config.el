;;; feature/spellcheck/config.el -*- lexical-binding: t; -*-

(defvar-local +spellcheck-immediately t
  "If non-nil, spellcheck the current buffer upon starting `flyspell-mode'.

Since spellchecking can be slow in some buffers, this can be disabled with:

  (setq-hook! 'TeX-mode-hook +spellcheck-immediately nil)")

;; `ispell'
(setq ispell-dictionary "english"
      ispell-list-command "--list"
      ispell-extr-args '("--dont-tex-check-comments"))

(after! ispell
  (when (equal (file-name-base ispell-program-name) "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")))


(def-package! flyspell ; built-in
  :defer t
  :init (add-hook 'flyspell-mode-hook #'+spellcheck|immediately)
  :config
  (defun +spellcheck|immediately ()
    "Spellcheck the buffer when `flyspell-mode' is enabled."
    (when (and flyspell-mode +spellcheck-immediately)
      (flyspell-buffer))))


(def-package! flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :config
  (cond ((featurep! :completion helm)
         (require 'flyspell-correct-helm))
        ((featurep! :completion ivy)
         (require 'flyspell-correct-ivy))
        ((require 'flyspell-correct-popup)
         (setq flyspell-popup-correct-delay 0.8)
         (define-key popup-menu-keymap [escape] #'keyboard-quit))))
