;;; checkers/spell/config.el -*- lexical-binding: t; -*-

(defvar ispell-dictionary "en_US")

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
       (defun +spell-remove-run-together-switch-for-aspell-h ()
         (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args))))

     (defun +spell-init-ispell-extra-args-a (orig-fun &rest args)
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
  (setq flyspell-issue-welcome-flag nil
        ;; Significantly speeds up flyspell, which would otherwise print
        ;; messages for every word when checking the entire buffer
        flyspell-issue-message-flag nil)

  (add-hook 'text-mode-hook #'flyspell-mode)
  (when (featurep! +everywhere)
    (add-hook! '(conf-mode-hook prog-mode-hook)
               #'flyspell-prog-mode))

  (add-hook! 'flyspell-mode-hook
    (defun +spell-inhibit-duplicate-detection-maybe-h ()
      "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
      (when (or (and (bound-and-true-p flycheck-mode)
                     (executable-find "proselint"))
                (featurep 'langtool))
        (setq-local flyspell-mark-duplications-flag nil))))

  ;; Ensure mode-local predicates declared with `set-flyspell-predicate!' are
  ;; used in their respective major modes.
  (add-hook 'flyspell-mode-hook #'+spell-init-flyspell-predicate-h)

  (map! :map flyspell-mouse-map
        "RET"     #'flyspell-correct-at-point
        [return]  #'flyspell-correct-at-point
        [mouse-1] #'flyspell-correct-at-point))


(use-package! flyspell-correct
  :commands flyspell-correct-at-point flyspell-correct-previous
  :config
  (cond ((and (featurep! :completion helm)
              (require 'flyspell-correct-helm nil t)))
        ((and (featurep! :completion ivy)
              (require 'flyspell-correct-ivy nil t)))
        ((require 'flyspell-correct-popup nil t)
         (setq flyspell-popup-correct-delay 0.8)
         (define-key popup-menu-keymap [escape] #'keyboard-quit))))
