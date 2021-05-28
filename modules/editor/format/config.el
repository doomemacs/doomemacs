;;; editor/format/config.el -*- lexical-binding: t; -*-

(defvar +format-on-save-enabled-modes
  '(not emacs-lisp-mode    ; elisp's mechanisms are good enough
        sql-mode           ; sqlformat is currently broken
        tex-mode           ; latexindent is broken
        latex-mode
        org-msg-edit-mode) ; doesn't need a formatter
  "A list of major modes in which to reformat the buffer upon saving.

If this list begins with `not', then it negates the list.
If it is `t', it is enabled in all modes.
If nil, it is disabled in all modes, the same as if the +onsave flag wasn't
  used at all.

Irrelevant if you do not have the +onsave flag enabled for this module.")

(defvar +format-preserve-indentation t
  "If non-nil, the leading indentation is preserved when formatting the whole
buffer. This is particularly useful for partials.

Indentation is always preserved when formatting regions.")

(defvar-local +format-with nil
  "Set this to explicitly use a certain formatter for the current buffer.")

(defvar +format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.

This can be set buffer-locally with `setq-hook!' to disable LSP formatting in
select buffers.")


;;
;;; Bootstrap

(defun +format-enable-for-lsp-on-save-maybe-h ()
  "Enable LSP formatter when LSP client is available."
  (remove-hook 'lsp-mode-hook #'+format-enable-for-lsp-on-save-maybe-h 'local)
  (cond ((not +format-with-lsp) nil)
        ((bound-and-true-p lsp-mode)
         (when (lsp-feature? "textDocument/formatting")
           (+format-enable-on-save-h))
         t)
        ((bound-and-true-p eglot--managed-mode)
         (when (eglot--server-capable :documentRangeFormattingProvider)
           (+format-enable-on-save-h))
         t)
        ((bound-and-true-p lsp--buffer-deferred)
         (add-hook 'lsp-mode-hook #'+format-enable-for-lsp-on-save-maybe-h
                   nil 'local)
         t)))

(defun +format-enable-on-save-maybe-h ()
  "Enable formatting on save in certain major modes.

This is controlled by `+format-on-save-enabled-modes'."
  (and (not (eq major-mode 'fundamental-mode))
       (cond ((booleanp +format-on-save-enabled-modes)
              +format-on-save-enabled-modes)
             ((eq (car-safe +format-on-save-enabled-modes) 'not)
              (not (memq major-mode (cdr +format-on-save-enabled-modes))))
             ((memq major-mode +format-on-save-enabled-modes))
             ((not (require 'format-all nil t))))
       (not (+format-enable-for-lsp-on-save-maybe-h))
       (let (current-prefix-arg) ; never prompt
         (car (format-all--probe)))
       (+format-enable-on-save-h)))

(when (featurep! +onsave)
  (add-hook 'after-change-major-mode-hook #'+format-enable-on-save-maybe-h))


;;
;;; Hacks

;; Allow a specific formatter to be used by setting `+format-with', either
;; buffer-locally or let-bound.
(advice-add #'format-all--probe :around #'+format-probe-a)

;; Doom uses a modded `format-all-buffer', which
;;   1. Enables partial reformatting (while preserving leading indentation),
;;   2. Applies changes via RCS patch, line by line, to protect buffer markers
;;      and avoid any jarring cursor+window scrolling.
(advice-add #'format-all-buffer--with :around #'+format-buffer-a)

;; format-all-mode "helpfully" raises an error when it doesn't know how to
;; format a buffer.
(add-to-list 'debug-ignored-errors "^Don't know how to format ")
