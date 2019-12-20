;;; ui/zen/config.el -*- lexical-binding: t; -*-

(defvar +zen-mixed-pitch-modes '(markdown-mode org-mode)
  "What major-modes to enable `mixed-pitch-mode' in with `writeroom-mode'.")


;;
;;; Packages

(after! writeroom-mode
  (setq writeroom-fullscreen-effect nil
        writeroom-maximize-window nil)

  ;; Adjust margins when text size is changed
  (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))


(use-package! mixed-pitch
  :hook (writeroom-mode . +zen-enable-mixed-pitch-mode-h)
  :config
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (mixed-pitch-mode (if writeroom-mode +1 -1))))

  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-date
            'org-special-keyword
            'org-property-value
            'org-ref-cite-face
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todo
            'org-indent
            'font-lock-comment-face
            'line-number
            'line-number-current-line))
