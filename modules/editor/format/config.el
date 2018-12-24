;;; editor/format/config.el -*- lexical-binding: t; -*-

(defvar +format-on-save-enabled-modes
  '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode)        ; sqlformat is currently broken
  "A list of major modes in which to reformat the buffer upon saving.

If this list begins with `not', then it negates the list.
If it is `t', it is enabled in all modes.
If nil, it is disabled in all modes, the same as if the +onsave flag wasn't
  used at all.

Irrelevant if you do not have the +onsave flag enabled for this module.")

(defvar +format-preserve-indentation t
  "If non-nil, the leading indentation is preserved when formatting the whole
buffer. This is particularly useful for partials.

Indentation is always preserved when formatting regions. ")

(defvar-local +format-with nil
  "Set this to explicitly use a certain formatter for the current buffer.")


;;
;; Bootstrap

(defun +format|enable-on-save-maybe ()
  "Enable formatting on save in certain major modes.

This is controlled by `+format-on-save-enabled-modes'."
  (unless (or (eq major-mode 'fundamental-mode)
              (cond ((booleanp +format-on-save-enabled-modes)
                     (null +format-on-save-enabled-modes))
                    ((eq (car +format-on-save-enabled-modes) 'not)
                     (memq major-mode (cdr +format-on-save-enabled-modes)))
                    ((not (memq major-mode +format-on-save-enabled-modes))))
              (not (require 'format-all nil t)))
    (add-hook 'before-save-hook #'+format|buffer nil t)))

(when (featurep! +onsave)
  (add-hook 'after-change-major-mode-hook #'+format|enable-on-save-maybe))


;;
;; Hacks

;; Allow a specific formatter to be used by setting `+format-with', either
;; buffer-locally or let-bound.
(advice-add #'format-all-probe :around #'+format*probe)

;; Doom uses a modded `format-all-buffer', which
;;   1. Doesn't move the cursorafter reformatting,
;;   2. Can reformat regions, rather than the entire buffer (while preserving
;;      leading indentation),
;;   3. Applies changes via RCS patch, line by line, as not to protect buffer
;;      markers and avoid any jarring cursor+window scrolling.
(advice-add #'format-all-buffer :override #'+format/buffer)
