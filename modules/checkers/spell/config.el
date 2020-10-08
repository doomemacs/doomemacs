;;; checkers/spell/config.el -*- lexical-binding: t; -*-

;;
;;; Ispell

;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(delq! 'ispell features)

(global-set-key [remap ispell-word] #'+spell/correct)

(after! ispell
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  ;; Enable either aspell, hunspell or enchant.
  ;;   If no module flags are given, enable either aspell, hunspell or enchant
  ;;     if their binary is found.
  ;;   If one of the flags `+aspell', `+hunspell' or `+enchant' is given,
  ;;     only enable that spell checker.
  (pcase (cond ((featurep! +aspell)   'aspell)
               ((featurep! +hunspell) 'hunspell)
               ((featurep! +enchant)  'enchant)
               ((executable-find "aspell")    'aspell)
               ((executable-find "hunspell")  'hunspell)
               ((executable-find "enchant-2") 'enchant))
    (`aspell
     (setq ispell-program-name "aspell"
           ispell-extra-args '("--sug-mode=ultra"
                               "--run-together"))

     (unless ispell-dictionary
       (setq ispell-dictionary "english"))
     (unless ispell-aspell-dict-dir
       (setq ispell-aspell-dict-dir
             (ispell-get-aspell-config-value "dict-dir")))
     (unless ispell-aspell-data-dir
       (setq ispell-aspell-data-dir
             (ispell-get-aspell-config-value "data-dir")))
     (unless ispell-personal-dictionary
       (setq ispell-personal-dictionary
             (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                               doom-etc-dir)))

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

    (`enchant
     (setq ispell-program-name "enchant-2"))

    (_ (doom-log "Spell checker not found. Either install `aspell', `hunspell' or `enchant'"))))


;;
;;; Implementations

(eval-if! (not (featurep! +flyspell))

    (use-package! spell-fu
      :when (executable-find "aspell")
      :hook (text-mode . spell-fu-mode)
      :general ([remap ispell-word] #'+spell/correct)
      :init
      (defvar +spell-correct-interface
        (cond ((featurep! :completion ivy)
               #'+spell-correct-ivy-fn)
              ((featurep! :completion helm)
               #'+spell-correct-helm-fn)
              (#'+spell-correct-generic-fn))
        "Function to use to display corrections.")

      (defvar +spell-excluded-faces-alist
        '((markdown-mode
           . (markdown-code-face
              markdown-html-attr-name-face
              markdown-html-attr-value-face
              markdown-html-tag-name-face
              markdown-link-face
              markdown-markup-face
              markdown-reference-face
              markdown-url-face))
          (org-mode
           . (org-block
              org-block-begin-line
              org-block-end-line
              org-code
              org-date
              org-formula
              org-latex-and-related
              org-link
              org-meta-line
              org-property-value
              org-ref-cite-face
              org-special-keyword
              org-tag
              org-todo
              org-todo-keyword-done
              org-todo-keyword-habt
              org-todo-keyword-kill
              org-todo-keyword-outd
              org-todo-keyword-todo
              org-todo-keyword-wait
              org-verbatim))
          (latex-mode
           . (font-latex-math-face
              font-latex-sedate-face
              font-lock-function-name-face
              font-lock-keyword-face
              font-lock-variable-name-face)))
        "Faces in certain major modes that spell-fu will not spellcheck.")

      (setq spell-fu-directory (concat doom-etc-dir "spell-fu"))
      (when (featurep! +everywhere)
        (add-hook! '(yaml-mode-hook
                     conf-mode-hook
                     prog-mode-hook)
                   #'spell-fu-mode))
      :config
      (map! :after spell-fu
            :map override
            :n [return]
            (cmds! (memq 'spell-fu-incorrect-face (face-at-point nil t))
                   #'+spell/correct))

      (defadvice! +spell--create-word-dict-a (_word words-file _action)
        :before #'spell-fu--word-add-or-remove
        (unless (file-exists-p words-file)
          (make-directory (file-name-directory words-file) t)
          (with-temp-file words-file
            (insert (format "personal_ws-1.1 %s 0\n" ispell-dictionary)))))

      (add-hook! 'spell-fu-mode-hook
        (defun +spell-init-excluded-faces-h ()
          "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
          (when-let (excluded (alist-get major-mode +spell-excluded-faces-alist))
            (setq-local spell-fu-faces-exclude excluded))))

      ;; TODO custom `spell-fu-check-range' function to reduce false positives
      ;;      more intelligently, or modify `spell-fu-word-regexp' to include
      ;;      non-latin charactersets.
      )

  (use-package! flyspell ; built-in
    :defer t
    :preface
    ;; `flyspell' is loaded at startup. In order to lazy load its config we need
    ;; to pretend it isn't loaded.
    (defer-feature! flyspell flyspell-mode flyspell-prog-mode)
    :init
    (add-hook! '(org-mode-hook
                 markdown-mode-hook
                 TeX-mode-hook
                 rst-mode-hook
                 mu4e-compose-mode-hook
                 message-mode-hook
                 git-commit-mode-hook)
               #'flyspell-mode)

    (when (featurep! +everywhere)
      (add-hook! '(yaml-mode-hook
                   conf-mode-hook
                   prog-mode-hook)
                 #'flyspell-prog-mode))

    :config
    (provide 'ispell) ; forcibly load ispell configs

    (setq flyspell-issue-welcome-flag nil
          ;; Significantly speeds up flyspell, which would otherwise print
          ;; messages for every word when checking the entire buffer
          flyspell-issue-message-flag nil)

    (add-hook! 'flyspell-mode-hook
      (defun +spell-inhibit-duplicate-detection-maybe-h ()
        "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
        (and (or (and (bound-and-true-p flycheck-mode)
                      (executable-find "proselint"))
                 (featurep 'langtool))
             (setq-local flyspell-mark-duplications-flag nil))))

    ;; Ensure mode-local predicates declared with `set-flyspell-predicate!' are
    ;; used in their respective major modes.
    (add-hook 'flyspell-mode-hook #'+spell-init-flyspell-predicate-h)

    (let ((flyspell-correct
           (cmds! (and (not (or mark-active (ignore-errors (evil-insert-state-p))))
                       (memq 'flyspell-incorrect (face-at-point nil t)))
                  #'flyspell-correct-at-point)))
      (map! :map flyspell-mouse-map
            "RET"    flyspell-correct
            [return] flyspell-correct
            [mouse-1] #'flyspell-correct-at-point)))


  (use-package! flyspell-correct
    :commands flyspell-correct-previous
    :general ([remap ispell-word] #'flyspell-correct-at-point)
    :config
    (cond ((and (featurep! :completion helm)
                (require 'flyspell-correct-helm nil t)))
          ((and (featurep! :completion ivy)
                (require 'flyspell-correct-ivy nil t)))
          ((require 'flyspell-correct-popup nil t)
           (setq flyspell-popup-correct-delay 0.8)
           (define-key popup-menu-keymap [escape] #'keyboard-quit))))


  (use-package! flyspell-lazy
    :after flyspell
    :config
    (setq flyspell-lazy-idle-seconds 1
          flyspell-lazy-window-idle-seconds 3)
    (flyspell-lazy-mode +1)))
