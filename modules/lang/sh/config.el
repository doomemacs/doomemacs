;;; lang/sh/config.el -*- lexical-binding: t; -*-

(defvar +sh-builtin-keywords
  '("cat" "cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git"
    "grep" "kill" "less" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd"
    "rm" "sleep" "sudo" "touch")
  "A list of common shell commands to be fontified especially in `sh-mode'.")


;;
;; Plugins
;;

(def-package! sh-script ; built-in
  :mode ("\\.zsh$"   . sh-mode)
  :mode ("\\.zunit$" . sh-mode)
  :mode ("/bspwmrc$" . sh-mode)
  :init
  (add-hook! sh-mode #'(flycheck-mode highlight-numbers-mode))
  :config
  (set! :electric 'sh-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
  (set! :repl 'sh-mode #'+sh/repl)

  (setq sh-indent-after-continuation 'always)

  ;; recognize function names with dashes in them
  (push '((sh . ((nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                 (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1))))
        sh-imenu-generic-expression)

  (defun +sh*silence-messages (orig-fn &rest args)
    "`sh-set-shell' is chatty about it setting up indentation rules. Shut up."
    (quiet! (apply orig-fn args)))
  (advice-add #'sh-set-shell :around #'+sh*silence-messages)

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `+sh-builtin-keywords')
  (font-lock-add-keywords
   'sh-mode `((+sh--match-variables-in-quotes
               (1 'font-lock-constant-face prepend)
               (2 'font-lock-variable-name-face prepend))
              (+sh--match-command-subst-in-quotes
               (1 'sh-quoted-exec prepend))
              (,(regexp-opt +sh-builtin-keywords 'words)
               (0 'font-lock-type-face append))))

  ;; autoclose backticks
  (sp-local-pair 'sh-mode "`" nil :unless '(sp-point-before-word-p sp-point-before-same-p))

  ;; sh-mode has file extensions checks for other shells, but not zsh, so...
  (defun +sh|detect-zsh ()
    (when (or (and buffer-file-name
                   (string-match-p "\\.zsh\\'" buffer-file-name))
              (save-excursion
                (goto-char (point-min))
                (looking-at-p "^#!.+zsh[$\\s-]")))
      (sh-set-shell "zsh")))
  (add-hook 'sh-mode-hook #'+sh|detect-zsh))


(def-package! company-shell
  :when (featurep! :completion company)
  :after sh-script
  :config
  (set! :company-backend 'sh-mode '(company-shell company-files))
  (setq company-shell-delete-duplicates t))

