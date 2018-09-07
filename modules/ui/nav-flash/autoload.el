;;; ui/nav-flash/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +nav-flash-blink-cursor (&rest _)
  "Blinks the current line in the current window, to make it clear where the
cursor has landed (typically after a large motion, like switching windows or
jumping to another part of the file)."
  (unless (minibufferp)
    (nav-flash-show)
    ;; only show in the current window
    (overlay-put compilation-highlight-overlay 'window (selected-window))))

;;;###autoload
(defun +nav-flash-blink-cursor-maybe (&rest _)
  "Like `+nav-flash-blink-cursor', but no-ops if in special-mode or term-mode,
or triggered from one of `+nav-flash-exclude-commands'."
  (unless (or (derived-mode-p 'special-mode 'term-mode)
              (memq this-command +nav-flash-exclude-commands))
    (+nav-flash-blink-cursor)))

;;;###autoload
(defun +nav-flash|delayed-blink-cursor (&rest _)
  "Like `+nav-flash-blink-cursor', but links after a tiny pause, in case it
isn't clear at run-time if the point will be in the correct window/buffer (like
for `org-follow-link-hook')."
  (run-at-time 0.1 nil #'+nav-flash|blink-cursor))

;;;###autoload
(defalias '+nav-flash|blink-cursor #'+nav-flash-blink-cursor)
;;;###autoload
(defalias '+nav-flash|blink-cursor-maybe #'+nav-flash-blink-cursor-maybe)

;;;###autoload
(defalias '+nav-flash*blink-cursor #'+nav-flash-blink-cursor-maybe)

;;;###autoload
(defun +nav-flash/blink-cursor (&rest _)
  "Blink current line using `nav-flash'."
  (interactive)
  (+nav-flash-blink-cursor-maybe))
