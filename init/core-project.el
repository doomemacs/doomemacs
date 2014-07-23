(require-packages
  '(ido-ubiquitous     ; enhances ido-everywhere
    projectile         ; project search (like ctrlp)
    helm               ; augments search of, well, anything
    grizzl             ; better searching engine for projectile
    ag                 ; the_silver_searcher support
    flx-ido            ; enhances ido's flex matching
    ido-vertical-mode  ; vertical listing for ido completion
    project-explorer   ; sidebar for navigation project files
	dired+
    ))

(diminish 'projectile-mode)

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
(add-to-list 'ido-ignore-files "\\`.DS_Store\\'")
(setq ido-use-faces nil
      ido-confirm-unique-completion t
      ido-case-fold t
      ido-enable-tramp-completion nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-enable-tramp-completion t
      ido-enable-last-directory-history t)

;; (add-to-list 'ido-ubiquitous-default-function-overrides '(disable exact "evil-ex"))

;;; Filters ido-matches setting acronynm matches in front of the results
(defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
  (if (and (fboundp 'smex-already-running) (smex-already-running)
           (> (length ido-text) 1))
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match (concat regex "[^-]*$") item) ;; strict match
              (add-to-list 'acronym-matches item)
            (if (string-match regex item) ;; appending relaxed match
                (add-to-list 'acronym-matches item t))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))

;;#projectile
(projectile-global-mode)
(setq projectile-completion-system 'grizzl
      projectile-enable-caching t)

;;#project-explorer
(setq pe/width 24)
(evil-set-initial-state 'project-explorer-mode 'emacs)
(add-hook 'project-explorer-mode-hook
          (lambda() (no-linum)))

;;
(provide 'core-project)
