;;; ui/pretty-code/autoload/pretty-code.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +pretty-code-symbols-alist '((t))
  "An alist containing a mapping of major modes to its value for
`prettify-symbols-alist'.")

;;;###autodef
(defun set-pretty-symbols! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in `+pretty-code-symbols',
and whose values are strings representing the text to be replaced with that
symbol. If the car of PLIST is nil, then unset any pretty symbols previously
defined for MODES.

This function accepts one special property:

  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+pretty-code-symbols'.

For example, the rule for emacs-lisp-mode is very simple:

  (set-pretty-symbols! 'emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+pretty-code-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

  (set-pretty-symbols! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (doom-enlist modes))
        (assq-delete-all mode +pretty-code-symbols-alist))
    (let (results)
      (while plist
        (let ((key (pop plist)))
          (if (eq key :alist)
              (prependq! results (pop plist))
            (when-let (char (plist-get +pretty-code-symbols key))
              (push (cons (pop plist) char) results)))))
      (dolist (mode (doom-enlist modes))
        (setf (alist-get mode +pretty-code-symbols-alist)
              (if-let (old-results (alist-get mode +pretty-code-symbols-alist))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))
