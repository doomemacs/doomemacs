(provide 'init-ido)

;; ido remaps its keys every time it's invoked, this screws with
;; custom mappings. So we've gotta neuter ido.
;; (defun ido-init-completion-maps ())
;; (setq ido-common-completion-map   (make-sparse-keymap)
;;       ido-file-dir-completion-map (make-sparse-keymap)
;;       ido-file-completion-map     (make-sparse-keymap)
;;       ido-buffer-completion-map   (make-sparse-keymap))

;; (set-keymap-parent ido-common-completion-map   minibuffer-local-map)
;; (set-keymap-parent ido-file-dir-completion-map ido-common-completion-map)
;; (set-keymap-parent ido-file-completion-map     ido-file-dir-completion-map)
;; (set-keymap-parent ido-buffer-completion-map   ido-common-completion-map)

(use-package ido-ubiquitous)
(use-package ido-vertical-mode)
(use-package flx-ido)

(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(add-to-list 'ido-ignore-files "\\`.DS_Store\\'")
(setq ido-use-faces nil
      ido-confirm-unique-completion t
      ido-case-fold t
      ido-enable-tramp-completion nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-enable-tramp-completion t
      ido-enable-last-directory-history t)
