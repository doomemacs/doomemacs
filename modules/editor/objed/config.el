;;; editor/objed/config.el -*- lexical-binding: t; -*-

(use-package! objed
  :after-call pre-command-hook
  :config
  ;; Prevent undo actions from exiting edit state
  (add-to-list 'objed-keeper-commands 'undo-tree-undo)
  (add-to-list 'objed-keeper-commands 'undo-tree-redo)
  (add-to-list 'objed-keeper-commands 'undo-tree-visualize)

  (defvar +objed--extra-face-remaps nil)

  (defadvice! +objed--add-face-remaps-a (&rest _)
    "Add extra face remaps when objed activates."
    :after 'objed--init
    (when (memq 'objed-hl (assq 'hl-line face-remapping-alist))
      (push (face-remap-add-relative 'solaire-hl-line-face 'objed-hl)
            +objed--extra-face-remaps)))

  (defadvice! +objed--remove-face-remaps-a (&rest _)
    "Remove extra face remaps when objed de-activates."
    :after 'objed--reset
    (unless (memq 'objed-hl (assq 'hl-line face-remapping-alist))
      (dolist (remap +objed--extra-face-remaps)
        (face-remap-remove-relative remap))
      (setq +objed--extra-face-remaps nil)))

  (unless (featurep! +manual)
    (objed-mode +1)))
