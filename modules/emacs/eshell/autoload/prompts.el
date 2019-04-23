;;; emacs/eshell/autoload/prompts.el -*- lexical-binding: t; -*-

;;;###autoload
(defface +eshell-prompt-pwd '((t :inherit font-lock-constant-face))
  "TODO"
  :group 'eshell)

;;;###autoload
(defface +eshell-prompt-git-branch '((t :inherit font-lock-builtin-face))
  "TODO"
  :group 'eshell)


(defun +eshell--current-git-branch ()
  (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                              if (string-match-p "^\*" match)
                              collect match))))
    (if (not (eq branch nil))
        (format " [%s]" (substring branch 2))
      "")))

;;;###autoload
(defun +eshell-default-prompt ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (concat (if (bobp) "" "\n")
          (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name (shrink-path-file pwd)))
                        'face '+eshell-prompt-pwd))
          (propertize (+eshell--current-git-branch)
                      'face '+eshell-prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))
