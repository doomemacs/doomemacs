(require-packages
  '(ido-ubiquitous     ; enhances ido-everywhere
    projectile         ; project search (like ctrlp)
    helm               ; augments search of, well, anything
    grizzl             ; better searching engine for projectile
    ag                 ; the_silver_searcher support
    sr-speedbar        ; speedbar, w/o the separate frame
    flx-ido            ; enhances ido's flex matching
    ido-vertical-mode  ; vertical listing for ido completion
    ))

;;#dired
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always

      ;; if there is a dired buffer displayed in the next window, use its
      ;; current subdir, instead of the current subdir of this dired buffer
      dired-dwim-target t)

;; ido remaps its keys every time it's invoked, this screws with
;; custom mappings. So we've gotta neuter ido.
(defun ido-init-completion-maps ())

(setq ido-common-completion-map   (make-sparse-keymap))
(setq ido-file-dir-completion-map (make-sparse-keymap))
(setq ido-file-completion-map     (make-sparse-keymap))
(setq ido-buffer-completion-map   (make-sparse-keymap))

(set-keymap-parent ido-common-completion-map   minibuffer-local-map)
(set-keymap-parent ido-file-dir-completion-map ido-common-completion-map)
(set-keymap-parent ido-file-completion-map     ido-file-dir-completion-map)
(set-keymap-parent ido-buffer-completion-map   ido-common-completion-map)

(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil
      ido-confirm-unique-completion t
      ido-case-fold t
      ido-enable-tramp-completion nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-enable-tramp-completion t
      ido-enable-last-directory-history t)

;;#projectile
(projectile-global-mode)
(setq projectile-completion-system 'grizzl
      projectile-enable-caching t)

;;#sr-speedbar
(setq speedbar-use-images nil)

(add-to-list 'ido-ignore-buffers "\\`\\*[^s].*\\*")
(add-to-list 'ido-ignore-files "\\`.DS_Store\\'")

;;
(provide 'core-project)
