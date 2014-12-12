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

(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(require 'flx-ido)

(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(setq ido-use-faces nil
      ido-confirm-unique-completion t
      ido-case-fold t
      ido-enable-tramp-completion nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-enable-tramp-completion t
      ido-enable-last-directory-history t
      ido-save-directory-list-file (concat my-tmp-dir "ido.last"))

(add-to-list 'ido-ignore-files "\\`.DS_Store$")
(setq ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
        "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
        "_region_" " output\\*$" "^TAGS$" "^\*Ido"))


(provide 'init-ido)
;;; init-ido.el ends here
