;;; term/eshell/autoload/prompts.el -*- lexical-binding: t; -*-

;;;###autoload
(defface +eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
  "TODO"
  :group 'eshell)

;;;###autoload
(defface +eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
  "TODO"
  :group 'eshell)


(defun +eshell--current-git-branch ()
  (cl-destructuring-bind (status . output)
      (doom-call-process "git" "name-rev" "--name-only" "HEAD")
    (if (equal status 0)
        (format " [%s]" output)
      "")))

;;;###autoload
(defun +eshell-default-prompt-fn ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (require 'shrink-path)
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
