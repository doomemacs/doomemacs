;;; ui/ligatures/autoload/ligatures.el -*- lexical-binding: t; -*-

;; DEPRECATED
;;;###autodef
(define-obsolete-function-alias 'set-pretty-symbols! 'set-ligatures! "3.0.0")

;;;###autodef
(defun set-ligatures! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in
`+ligatures-extra-symbols', and whose values are strings representing the text
to be replaced with that symbol.

If the car of PLIST is nil, then unset any
pretty symbols and ligatures previously defined for MODES.

For example, the rule for emacs-lisp-mode is very simple:

  (set-ligatures! \\='emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
associated with :lambda in `+ligatures-extra-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

  (set-ligatures! \\='emacs-lisp-mode nil)

Note that this will keep all ligatures in `+ligatures-prog-mode-list' active, as
`emacs-lisp-mode' is derived from `prog-mode'."
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (ensure-list modes))
        (delq! mode +ligatures-extra-alist 'assq))
    (let ((results))
      (while plist
        (let ((key (pop plist)))
            (when-let (char (plist-get +ligatures-extra-symbols key))
              (push (cons (pop plist) char) results))))
      (dolist (mode (ensure-list modes))
        (setf (alist-get mode +ligatures-extra-alist)
              (if-let (old-results (alist-get mode +ligatures-extra-alist))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))

;;;###autodef
(defun set-font-ligatures! (modes &rest ligatures)
  "Associates string patterns with ligatures in certain major-modes.

  MODES is a major mode symbol or a list of them.
  LIGATURES is a list of ligatures that should be handled by the font,
    like \"==\" or \"-->\". LIGATURES is a list of strings.

For example, the rule for emacs-lisp-mode is very simple:

  (set-font-ligatures! \\='emacs-lisp-mode \"->\")

This will ligate \"->\" into the arrow of choice according to your font.

Font ligatures can be unset for emacs-lisp-mode with:

  (set-font-ligatures! \\='emacs-lisp-mode nil)

Note that this will keep all ligatures in `+ligatures-prog-mode-list' active, as
`emacs-lisp-mode' is derived from `prog-mode'."
  (declare (indent defun))
  ;; NOTE: Doom enforces `ligature-composition-table' to have a single mode per key in the alist.
  ;; This is less efficient than what ligature.el can do (i.e. use a list of modes, or `t' as a key),
  ;; but holding this invariant allows resetting with `(set-font-ligatures! 'mode nil)` to work reliably.
  (if (null ligatures)
      (dolist (mode (ensure-list modes))
        (delq! mode ligature-composition-table 'assq))
    (after! ligature
       (dolist (mode (ensure-list modes))
         (setq ligature-ignored-major-modes (delq mode ligature-ignored-major-modes))
         (ligature-set-ligatures mode ligatures)))))


