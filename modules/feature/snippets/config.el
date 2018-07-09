;;; feature/snippets/config.el -*- lexical-binding: t; -*-

(defvar +snippets-dir (expand-file-name "snippets/" doom-private-dir)
  "Directory where `yasnippet' will search for your private snippets.")


;;
;; Plugins
;;

(def-package! yasnippet
  :commands (yas-minor-mode-on yas-expand yas-expand-snippet yas-lookup-snippet
             yas-insert-snippet yas-new-snippet yas-visit-snippet-file)
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

  ;; Register `def-project-mode!' modes with yasnippet. This enables project
  ;; specific snippet libraries (e.g. for Laravel, React or Jekyll projects).
  (add-hook 'doom-project-hook #'+snippets|enable-project-modes)

  ;; Exit snippets on ESC from normal mode
  (add-hook 'doom-escape-hook #'yas-abort-snippet)

  (after! smartparens
    ;; tell smartparens overlays not to interfere with yasnippet keybinds
    (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))

  (when (featurep! :feature evil)
    ;; evil visual-mode integration for `yas-insert-snippet'
    (define-key yas-minor-mode-map [remap yas-insert-snippet] #'+snippets/expand-on-region)))


;; `auto-yasnippet'
(setq aya-persist-snippets-dir (concat doom-etc-dir "auto-snippets/"))
