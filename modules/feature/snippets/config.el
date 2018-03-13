;;; feature/snippets/config.el -*- lexical-binding: t; -*-

;; Snippets! I've thrown together a few hacks to make `yasnippet' and `evil'
;; behave together.

(def-package! yasnippet
  :commands (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet
             yas-lookup-snippet yas-insert-snippet yas-new-snippet
             yas-visit-snippet-file snippet-mode)
  :preface
  (defvar yas-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (when (featurep! :feature evil)
        (define-key map [remap yas-insert-snippet] #'+snippets/expand-on-region))
      map))

  :init
  ;; Ensure `yas-reload-all' is called as late as possible. Other modules could
  ;; have additional configuration for yasnippet. For example, file-templates.
  (add-transient-hook! 'yas-minor-mode-hook (yas-reload-all))

  (add-hook! (text-mode prog-mode snippet-mode)
    #'yas-minor-mode-on)

  :config
  (setq yas-verbosity (if doom-debug-mode 3 0)
        yas-also-auto-indent-first-line t
        yas-prompt-functions (delq 'yas-dropdown-prompt yas-prompt-functions)
        ;; Allow nested snippets
        yas-triggers-in-field t)

  ;; Allows project-specific snippets
  (defun +snippets|enable-project-modes (mode &rest _)
    "Enable snippets for project modes."
    (if (symbol-value mode)
        (yas-activate-extra-mode mode)
      (yas-deactivate-extra-mode mode)))
  (add-hook 'doom-project-hook #'+snippets|enable-project-modes)

  ;; fix an error caused by smartparens interfering with yasnippet bindings
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)

  ;; Exit snippets on ESC from normal mode
  (add-hook 'doom-escape-hook #'yas-abort-snippet))


(def-package! auto-yasnippet
  :commands (aya-create aya-expand aya-open-line aya-persist-snippet)
  :config
  (setq aya-persist-snippets-dir (concat doom-local-dir "auto-snippets/")))

