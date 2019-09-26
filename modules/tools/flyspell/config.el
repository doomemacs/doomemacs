;;; tools/flyspell/config.el -*- lexical-binding: t; -*-

(defvar-local +flyspell-immediately nil
  "If non-nil, spellcheck the current buffer upon starting `flyspell-mode'.

Since spellchecking can be slow in some buffers, this can be disabled with:

  (setq-hook! 'TeX-mode-hook +flyspell-immediately nil)")


;;
;;; Packages

(after! ispell
  (add-to-list 'ispell-extra-args "--dont-tex-check-comments")

  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  ;; Enable either aspell or hunspell.
  ;;   If no module flags are given, enable either aspell or hunspell if their
  ;;     binary is found.
  ;;   If one of the flags `+aspell' or `+hunspell' is given, only enable that
  ;;     spell checker.
  (pcase (cond ((featurep! +aspell)   'aspell)
               ((featurep! +hunspell) 'hunspell)
               ((executable-find "aspell")   'aspell)
               ((executable-find "hunspell") 'hunspell))
    (`aspell
     (setq ispell-program-name "aspell"
           ispell-extra-args '("--sug-mode=ultra" "--run-together"))

     (add-hook! 'text-mode-hook
       (defun +flyspell-remove-run-together-switch-for-aspell-h ()
         (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args))))

     (defun +flyspell-setup-ispell-extra-args-a (orig-fun &rest args)
       :around '(ispell-word flyspell-auto-correct-word)
       (let ((ispell-extra-args (remove "--run-together" ispell-extra-args)))
         (ispell-kill-ispell t)
         (apply orig-fun args)
         (ispell-kill-ispell t))))

    (`hunspell
     (setq ispell-program-name "hunspell"))

    (_ (doom-log "Spell checker not found. Either install `aspell' or `hunspell'"))))


;;;###package flyspell
(progn ; built-in
  (setq flyspell-issue-welcome-flag nil)

  (when (featurep! +prog)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode))

  (add-hook! 'flyspell-mode-hook
    (defun +flyspell-inhibit-duplicate-detection-maybe-h ()
      "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
      (when (or (and (bound-and-true-p flycheck-mode)
                     (executable-find "proselint"))
                (featurep 'langtool))
        (setq-local flyspell-mark-duplications-flag nil))))

  (add-hook! 'flyspell-mode-hook
    (defun +flyspell-immediately-h ()
      "Spellcheck the buffer when `flyspell-mode' is enabled."
      (when (and flyspell-mode +flyspell-immediately)
        (flyspell-buffer))))

  ;; Ensure mode-local predicates declared with `set-flyspell-predicate!' are
  ;; used in their respective major modes.
  (add-hook 'flyspell-mode-hook #'+flyspell-init-predicate-h))


(use-package! flyspell-correct
  :commands flyspell-correct-word-generic flyspell-correct-previous-word-generic
  :config
  (cond ((and (featurep! :completion helm)
              (require 'flyspell-correct-helm nil t)))
        ((and (featurep! :completion ivy)
              (require 'flyspell-correct-ivy nil t)))
        ((require 'flyspell-correct-popup nil t)
         (setq flyspell-popup-correct-delay 0.8)
         (define-key popup-menu-keymap [escape] #'keyboard-quit))))
