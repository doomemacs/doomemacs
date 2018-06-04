;;; feature/snippets/config.el -*- lexical-binding: t; -*-

(defvar +snippets-dir (expand-file-name "snippets/" doom-private-dir)
  "Directory where `yasnippet' will search for your private snippets.")


;;
;; Plugins
;;

(def-package! yasnippet
  :commands (yas-minor-mode-on yas-expand yas-expand-snippet yas-lookup-snippet
             yas-insert-snippet yas-new-snippet yas-visit-snippet-file)
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
        yas-prompt-functions (delq #'yas-dropdown-prompt yas-prompt-functions)
        yas-triggers-in-field t)  ; Allow nested snippets

  (add-to-list 'yas-snippet-dirs '+snippets-dir nil #'eq)

  (defun +snippets|enable-project-modes (mode &rest _)
    "Automatically enable snippet libraries for project minor modes defined with
`def-project-mode!'."
    (if (symbol-value mode)
        (yas-activate-extra-mode mode)
      (yas-deactivate-extra-mode mode)))
  (add-hook 'doom-project-hook #'+snippets|enable-project-modes)

  ;; Exit snippets on ESC from normal mode
  (add-hook 'doom-escape-hook #'yas-abort-snippet)

  (after! smartparens
    ;; fix an error caused by smartparens interfering with yasnippet bindings
    (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)))


;; `auto-yasnippet'
(setq aya-persist-snippets-dir (concat doom-local-dir "auto-snippets/"))
