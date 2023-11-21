;;; editor/format/config.el -*- lexical-binding: t; -*-

(defvar +format-on-save-disabled-modes
  '(sql-mode           ; sqlformat is currently broken
    tex-mode           ; latexindent is broken
    latex-mode
    org-msg-edit-mode) ; doesn't need a formatter
  "A list of major modes in which to not reformat the buffer upon saving.
If it is t, it is disabled in all modes, the same as if the +onsave flag
  wasn't used at all.
If nil, formatting is enabled in all modes.
Irrelevant if you do not have the +onsave flag enabled for this module.")

(defvar +format-preserve-indentation t
  "If non-nil, the leading indentation is preserved when formatting the whole
buffer. This is particularly useful for partials.

Indentation is always preserved when formatting regions.")

(defvar +format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.

This can be set buffer-locally with `setq-hook!' to disable LSP formatting in
select buffers.
This has no effect on the +onsave flag, apheleia will always be used there.")

(defvaralias '+format-with 'apheleia-formatter
  "Set this to explicitly use a certain formatter for the current buffer.")


;;
;;; Bootstrap

(when (modulep! +onsave)
  (add-hook 'doom-first-file-hook #'apheleia-global-mode))

(defun +format-maybe-inhibit-h ()
  "Check if formatting should be disabled for current buffer.
This is controlled by `+format-on-save-disabled-modes'."
  (or (eq major-mode 'fundamental-mode)
      (string-blank-p (buffer-name))
      (eq +format-on-save-disabled-modes t)
      (not (null (memq major-mode +format-on-save-disabled-modes)))))


(after! apheleia
  (add-to-list 'doom-debug-variables '(apheleia-log-only-errors . nil))

  (when (modulep! +onsave)
    (add-to-list 'apheleia-inhibit-functions #'+format-maybe-inhibit-h)))


;;
;;; Hacks

(defadvice! +format--inhibit-reformat-on-prefix-arg-a (orig-fn &optional arg)
  "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
  :around #'save-buffer
  (let ((apheleia-mode (and apheleia-mode (memq arg '(nil 1)))))
    (funcall orig-fn)))

(add-hook!
 'apheleia-post-format-hook
 ;; HACK `web-mode' doesn't update syntax highlighting after arbitrary buffer
 ;;      modifications, so we must trigger refontification manually.
 (defun +format--fix-web-mode-fontification-h ()
   (when (eq major-mode 'web-mode)
     (setq web-mode-fontification-off nil)
     (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
       (save-excursion
         (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end)))))

 (defun +format--refresh-git-gutter-h ()
   (when (fboundp '+vc-gutter-update-h)
     (+vc-gutter-update-h))))
