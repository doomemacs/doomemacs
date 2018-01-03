;;; feature/snippets/config.el -*- lexical-binding: t; -*-

;; Snippets! I've thrown together a few hacks to make `yasnippet' and `evil'
;; behave together.

(def-package! yasnippet
  :commands (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet
             yas-lookup-snippet yas-insert-snippet yas-new-snippet
             yas-visit-snippet-file snippet-mode)
  :preface
  (defvar yas-minor-mode-map (make-sparse-keymap))

  :init
  ;; Ensure `yas-reload-all' is called as late as possible. Other modules could
  ;; have additional configuration for yasnippet. For example, file-templates.
  (add-transient-hook! 'yas-minor-mode-hook (yas-reload-all))

  (add-hook! (text-mode prog-mode snippet-mode)
    #'yas-minor-mode-on)

  :config
  (setq yas-verbosity (if doom-debug-mode 3 0)
        yas-also-auto-indent-first-line t
        yas-prompt-functions (delq 'yas-dropdown-prompt yas-prompt-functions)
        ;; Allow nested snippets
        yas-triggers-in-field t)

  ;; Allows project-specific snippets
  (defun +snippets|enable-project-modes (mode &rest _)
    "Enable snippets for project modes."
    (if (symbol-value mode)
        (yas-activate-extra-mode mode)
      (yas-deactivate-extra-mode mode)))
  (add-hook 'doom-project-hook #'+snippets|enable-project-modes)

  ;; fix an error caused by smartparens interfering with yasnippet bindings
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)

  ;; Exit snippets on ESC from normal mode
  (add-hook '+evil-esc-hook #'yas-abort-snippet)

  ;; Monkey patch yas-expand-snippet until #883 is resolved. See
  ;; https://github.com/joaotavora/yasnippet/issues/883
  (defun +snippets*yas-expand-snippet (content &optional start end expand-env)
    "Expand snippet CONTENT at current point.

Text between START and END will be deleted before inserting
template.  EXPAND-ENV is a list of (SYM VALUE) let-style dynamic bindings
considered when expanding the snippet."
    (cl-assert (and yas-minor-mode (memq 'yas--post-command-handler post-command-hook))
               nil "[yas] `yas-expand-snippet' needs properly setup `yas-minor-mode'")
    (run-hooks 'yas-before-expand-snippet-hook)
    (let* ((yas-selected-text (or yas-selected-text
                                  (and (region-active-p)
                                       (buffer-substring-no-properties (region-beginning)
                                                                       (region-end)))))
           (start (or start (and (region-active-p) (region-beginning)) (point)))
           (end (or end (and (region-active-p) (region-end)) (point)))
           (to-delete (and start end (buffer-substring-no-properties start end)))
           snippet)
      (goto-char start)
      (setq yas--indent-original-column (current-column))
      (when (and to-delete (> end start))
        (delete-region start end))
      (cond ((listp content) (yas--eval-for-effect content))
            (t
             (setq yas--start-column (current-column))
             (let ((yas--inhibit-overlay-hooks t))
               (setq snippet
                     (yas--snippet-create content expand-env start (point))))
             (let ((existing-field (and yas--active-field-overlay
                                        (overlay-buffer yas--active-field-overlay)
                                        (overlay-get yas--active-field-overlay 'yas--field))))
               (when existing-field
                 (setf (yas--snippet-previous-active-field snippet) existing-field)
                 (yas--advance-end-maybe existing-field (overlay-end yas--active-field-overlay))))
             (unless (yas--snippet-fields snippet)
               (yas-exit-snippet snippet))
             (let ((first-field (car (yas--snippet-fields snippet))))
               (when first-field
                 (sit-for 0)
                 (yas--letenv (yas--snippet-expand-env snippet)
                   (yas--move-to-field snippet first-field))
                 (when (and (eq (yas--field-number first-field) 0)
                            (> (length (yas--field-text-for-display
                                        first-field))
                               0))
                   (setq deactivate-mark nil))))
             (yas--message 4 "snippet %d expanded." (yas--snippet-id snippet))
             t))))
  (advice-add #'yas-expand-snippet :override #'+snippets*yas-expand-snippet))


(def-package! auto-yasnippet
  :commands (aya-create aya-expand aya-open-line aya-persist-snippet)
  :config
  (setq aya-persist-snippets-dir (concat doom-local-dir "auto-snippets/")))

