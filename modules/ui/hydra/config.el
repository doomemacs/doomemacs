;;; ui/hydra/config.el -*- lexical-binding: t; -*-

(use-package! hydra-examples
  :commands (hydra-move-splitter-up
             hydra-move-splitter-down
             hydra-move-splitter-right
             hydra-move-splitter-left))

;;;###package hydra
(setq lv-use-separator t)

(defadvice! +hydra--inhibit-window-switch-hooks-a (orig-fn)
  :around #'lv-window
  (let ((doom-inhibit-switch-window-hooks t))
    (funcall orig-fn)))
