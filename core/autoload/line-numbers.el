;;; core/autoload/line-numbers.el -*- lexical-binding: t; -*-
;;;###if (not EMACS26+)

;; This was lifted out of the display-line-numbers library in Emacs 26.1 and
;; modified to use nlinum for Emacs 25.x users. It should be removed should
;; Emacs 25 support be removed.

;;;###autoload
(defvar display-line-numbers t
  "Non-nil means display line numbers.

If the value is t, display the absolute number of each line of a buffer
shown in a window.  Absolute line numbers count from the beginning of
the current narrowing, or from buffer beginning.  If the value is
relative, display for each line not containing the window's point its
relative number instead, i.e. the number of the line relative to the
line showing the window's point.

In either case, line numbers are displayed at the beginning of each
non-continuation line that displays buffer text, i.e. after each newline
character that comes from the buffer.  The value visual is like
relative but counts screen lines instead of buffer lines.  In practice
this means that continuation lines count as well when calculating the
relative number of a line.

Lisp programs can disable display of a line number of a particular
buffer line by putting the display-line-numbers-disable text property
or overlay property on the first visible character of that line.")

(defgroup display-line-numbers nil "Display line number preferences"
 :group 'emacs)

;;;###autoload
(defcustom display-line-numbers-type t
  "The default type of line numbers to use in `display-line-numbers-mode'.
See `display-line-numbers' for value options."
  :type '(choice (const :tag "Relative line numbers" relative)
                 (const :tag "Relative visual line numbers" visual)
                 (other :tag "Absolute line numbers" t)))

;;;###autoload
(defcustom display-line-numbers-grow-only nil
  "If non-nil, do not shrink line number width."
  :type 'boolean)

;;;###autoload
(defcustom display-line-numbers-width-start nil
  "If non-nil, count number of lines to use for line number width.
When `display-line-numbers-mode' is turned on,
`display-line-numbers-width' is set to the minimum width necessary
to display all line numbers in the buffer."
  :type 'boolean)

;;;###autoload
(defun line-number-display-width ()
  "Return the width used for displaying line numbers in the
selected window."
  (length (save-excursion (goto-char (point-max))
                          (format-mode-line "%l"))))

(defun display-line-numbers-update-width ()
  "Prevent the line number width from shrinking."
  (let ((width (line-number-display-width)))
    (when (> width (or display-line-numbers-width 1))
      (setq display-line-numbers-width width))))

;;;###autoload
(define-minor-mode display-line-numbers-mode
  "Toggle display of line numbers in the buffer.
This uses `display-line-numbers' internally.

To change the type of line numbers displayed by default,
customize `display-line-numbers-type'.  To change the type while
the mode is on, set `display-line-numbers' directly."
  :lighter nil
  (cond ((null display-line-numbers-type))
        ((eq display-line-numbers-type 'relative)
         (if display-line-numbers-mode
             (nlinum-relative-off)
           (nlinum-relative-on)))
        ((nlinum-mode (if display-line-numbers-mode +1 -1)))))

(defun display-line-numbers--turn-on ()
  "Turn on `display-line-numbers-mode'."
  (unless (or (minibufferp)
              ;; taken from linum.el
              (and (daemonp) (null (frame-parameter nil 'client))))
    (display-line-numbers-mode)))

;;;###autoload
(define-globalized-minor-mode global-display-line-numbers-mode
  display-line-numbers-mode display-line-numbers--turn-on)
