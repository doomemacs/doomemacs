;;; lang/gdscript/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "project.godot"))

(after! which-key
  (push '((nil . "gdscript-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist))

(use-package! gdscript-mode
  :config
  (set-lookup-handlers! 'gdscript-mode
    :documentation #'gdscript-docs-browse-symbol-at-point)
  (map! :localleader
        :map gdscript-mode-map
        "o" #'gdscript-godot-open-project-in-editor
        "D" #'gdscript-godot-run-project-debug
        "f" #'gdscript-format-region
        "F" #'gdscript-format-buffer
        "d" #'gdscript-godot-run-current-scene-debug
        "R" #'gdscript-godot-run-project
        "r" #'gdscript-godot-run-current-scene
        "s" #'gdscript-godot-run-current-script
        "a" #'gdscript-docs-browse-api))
