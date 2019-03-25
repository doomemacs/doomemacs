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
           ispell-extra-args (remove "--run-together" ispell-extra-args))

         (defun +flyspell*setup-ispell-extra-args (orig-fun &rest args)
           (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
             (ispell-kill-ispell t)
             (apply orig-fun args)
             (ispell-kill-ispell t)))
         (advice-add #'ispell-word :around #'+flyspell*setup-ispell-extra-args)
         (advice-add #'flyspell-auto-correct-word :around #'+flyspell*setup-ispell-extra-args))

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

  (add-to-list 'ispell-extra-args "--dont-tex-check-comments"))


;; `flyspell' (built-in)
(progn
  (setq flyspell-issue-welcome-flag nil)

  (defun +flyspell|inhibit-duplicate-detection-maybe ()
    "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
    (when (or (executable-find "proselint")
              (featurep 'langtool))
      (setq-local flyspell-mark-duplications-flag nil)))
  (add-hook 'flyspell-mode-hook #'+flyspell|inhibit-duplicate-detection-maybe)

  (defun +flyspell|immediately ()
    "Spellcheck the buffer when `flyspell-mode' is enabled."
    (when (and flyspell-mode +flyspell-immediately)
      (flyspell-buffer)))
  (add-hook 'flyspell-mode-hook #'+flyspell|immediately)

  ;; Ensure mode-local predicates declared with `set-flyspell-predicate!' are
  ;; used in their respective major modes.
  (add-hook 'flyspell-mode-hook #'+flyspell|init-predicate))


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
