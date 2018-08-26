;;; editor/format/config.el -*- lexical-binding: t; -*-

(defvar +format-on-save-enabled-modes t
  "A list of major modes in which to enable `format-all-mode'.

This mode will auto-format buffers when you save them.

If this list begins with `not', then it negates the list.
If it is `t', it is enabled in all modes.
If nil, it is disabled in all modes, the same as if the +onsave flag wasn't
  used at all.")


;;
;; Plugins
;;

(defun +format|enable-on-save-maybe ()
  "Enable `format-all-mode' in buffers. See `+format-on-save-enabled-modes' to
control which major modes to target."
  (unless (or (eq major-mode 'fundamental-mode)
              (cond ((booleanp +format-on-save-enabled-modes)
                     (null +format-on-save-enabled-modes))
                    ((eq (car +format-on-save-enabled-modes) 'not)
                     (memq major-mode (cdr +format-on-save-enabled-modes)))
                    ((not (memq major-mode +format-on-save-enabled-modes))))
              (not (require 'format-all nil t))
              (not (format-all-probe)))
    (add-hook 'before-save-hook #'+format|on-save nil t)))

(when (featurep! +onsave)
  (add-hook 'after-change-major-mode-hook #'+format|enable-on-save-maybe))
