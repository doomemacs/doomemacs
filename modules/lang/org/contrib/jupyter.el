;;; lang/org/contrib/jupyter.el -*- lexical-binding: t; -*-
;;;###if (modulep! +jupyter)
;; TODO: Move to :tools jupyter

(use-package! jupyter-repl
  :defer t
  :config
  ;; HACK: If the user is anywhere but the last prompt, typing should move them
  ;;   there instead of unhelpfully spewing read-only errors at them.
  ;; REVIEW: Upstream this (maybe?)
  (defun +jupyter--move-cursor-to-prompt-h ()
    (and (eq this-command 'self-insert-command)
         (> (save-excursion
              (goto-char (point-max))
              (jupyter-repl-cell-code-beginning-position))
            (point))
         (goto-char (point-max))))

  (add-hook! 'jupyter-repl-mode-hook
    (defun +jupyter--init-move-cursor-to-prompt-h ()
      (add-hook 'pre-command-hook #'+jupyter--move-cursor-to-prompt-h
                nil t))))


(use-package! ob-jupyter
  :defer t
  :init
  (after! ob-async
    (pushnew! ob-async-no-async-languages-alist
              "jupyter-python"
              "jupyter-julia"
              "jupyter-R"))

  (after! org-src
    (dolist (lang '(python julia R))
      (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                  org-src-lang-modes :key #'car)))

  (add-hook! '+org-babel-load-functions
    (defun +org-babel-load-jupyter-h (lang)
      (when (string-prefix-p "jupyter-" (symbol-name lang))
        (require 'jupyter)
        (let* ((lang-name (symbol-name lang))
               (lang-tail (string-remove-prefix "jupyter-" lang-name)))
          (and (not (assoc lang-tail org-src-lang-modes))
               (require (intern (format "ob-%s" lang-tail))
                        nil t)
               (add-to-list 'org-src-lang-modes (cons lang-name (intern lang-tail)))))
        (with-demoted-errors "Jupyter: %s"
          (require lang nil t)
          (require 'ob-jupyter nil t)))))
  :config
  (defadvice! +org--ob-jupyter-initiate-session-a (&rest _)
    :after #'org-babel-jupyter-initiate-session
    (unless (bound-and-true-p jupyter-org-interaction-mode)
      (jupyter-org-interaction-mode)))

  (require 'tramp))
