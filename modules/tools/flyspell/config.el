;;; tools/flyspell/config.el -*- lexical-binding: t; -*-

(defvar-local +flyspell-immediately t
  "If non-nil, spellcheck the current buffer upon starting `flyspell-mode'.

Since spellchecking can be slow in some buffers, this can be disabled with:

  (setq-hook! 'TeX-mode-hook +flyspell-immediately nil)")


;;
;; Packages

(after! ispell
  (setq-default ispell-dictionary "english")

  (cond ((executable-find "aspell")
         (setq ispell-program-name "aspell"
               ispell-extra-args '("--sug-mode=ultra" "--run-together"))

         (setq-hook! 'text-mode-hook
           ispell-extra-args (remove "--run-together" ispell-extra-args)))

        ((executable-find "hunspell")
         (setq ispell-program-name "hunspell"
               ;; Don't use `ispell-cmd-args', it isn't respected with hunspell.
               ;; Hack ispell-local-dictionary-alist instead.
               ispell-dictionary-alist
               `((,ispell-local-dictionary
                  "[[:alpha:]]"
                  "[^[:alpha:]]"
                  "[']"
                  nil
                  ("-d" ,ispell-local-dictionary)
                  nil
                  utf-8)))))

  (add-to-list 'ispell-extra-args "--dont-tex-check-comments")

  (defun +flyspell*setup-ispell-extra-args (orig-fun &rest args)
    (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
      (ispell-kill-ispell t)
      (apply orig-fun args)
      (ispell-kill-ispell t)))
  (advice-add #'ispell-word :around #'+flyspell*setup-ispell-extra-args)
  (advice-add #'flyspell-auto-correct-word :around #'+flyspell*setup-ispell-extra-args))


;; `flyspell' (built-in)
(setq flyspell-issue-welcome-flag nil)

(defun +flyspell|immediately ()
  "Spellcheck the buffer when `flyspell-mode' is enabled."
  (when (and flyspell-mode +flyspell-immediately)
    (flyspell-buffer)))
(add-hook 'flyspell-mode-hook #'+flyspell|immediately)


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
