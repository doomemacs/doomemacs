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


(ido-mode 1)
(ido-everywhere 1)

(use-package ido-vertical-mode  :config (ido-vertical-mode 1))
(use-package ido-ubiquitous     :config (ido-ubiquitous-mode 1))
(use-package flx-ido            :config (flx-ido-mode 1))

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

; sort ido filelist by mtime instead of alphabetically
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))


(provide 'init-ido)
;;; init-ido.el ends here
