;;; lang/gdscript/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "project.godot"))


;;
;;; Packages

(use-package! gdscript-mode
  :defer t
  :config
  (set-lookup-handlers! 'gdscript-mode
    :documentation #'gdscript-docs-browse-symbol-at-point)
  (map! :localleader
        :map gdscript-mode-map

        (:prefix ("r" . "run")
         "e" #'gdscript-godot-open-project-in-editor
         "p" #'gdscript-godot-run-project
         "d" #'gdscript-godot-run-project-debug
         "s" #'gdscript-godot-run-current-scene)

        (:prefix ("h" . "help")
         "b" #'gdscript-docs-browse-api
         "f" #'gdscript-docs-browse-symbol-at-point)

        (:prefix ("f" . "format")
         "b" #'gdscript-format-buffer
         "r" #'gdscript-format-region))

  (when (featurep! +lsp)
    (add-hook 'gdscript-mode-local-vars-hook #'lsp!)))
