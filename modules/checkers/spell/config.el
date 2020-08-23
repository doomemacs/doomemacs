;;; checkers/spell/config.el -*- lexical-binding: t; -*-

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
        org-verbatim)))
  "Faces in certain major modes that spell-fu will not spellcheck.")


;;
;;; Packages

(global-set-key [remap ispell-word] #'+spell/correct)

(after! ispell
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
           ispell-extra-args '("--sug-mode=ultra"
                               "--run-together"
                               "--dont-tex-check-comments"))

     (unless ispell-dictionary
       (setq ispell-dictionary "en"))
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

    (_ (doom-log "Spell checker not found. Either install `aspell' or `hunspell'"))))


(use-package! spell-fu
  :when (executable-find "aspell")
  :hook (text-mode . spell-fu-mode)
  :init
  (setq spell-fu-directory (concat doom-etc-dir "spell-fu"))
  (when (featurep! +everywhere)
    (add-hook! '(yaml-mode-hook
                 conf-mode-hook
                 prog-mode-hook)
               #'spell-fu-mode))
  :config
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

  ;; TODO custom `spell-fu-check-range' function to exclude URLs from links or
  ;;      org-src blocks more intelligently.
  )
