;;; ui/pretty-code/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +pretty-code-enabled-modes
  '(c++-mode-hook
    c-mode-hook
    elm-mode
    emacs-lisp-mode
    js2-mode
    org-mode
    python-mode
    typescript-mode
    web-mode)
  "List of major modes in which `prettify-symbols-mode' should be enabled.")

;;;###autoload
(defvar +pretty-code-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end " "
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :tuple         "⨂"
    :pipe          "")
  "Options plist for `pretty-code-get-pairs'.")

;; When you get to the right edge, it goes back to how it normally prints
;;;###autoload
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;;###autodef
(defun set-pretty-symbols! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in `+pretty-code-symbols',
and whose values are strings representing the text to be replaced with that
symbol.

For example, the rule for emacs-lisp-mode is very simple:

  (set-pretty-symbols! 'emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+pretty-code-symbols'."
  (declare (indent 1))
  (dolist (mode (doom-enlist modes))
    (let ((fn (intern (format "+pretty-code|init-%s" mode))))
      (fset fn
            (lambda ()
              (when (and (eq major-mode mode)
                         (memq major-mode +pretty-code-enabled-modes))
                (let (results prop icon)
                  (while plist
                    (let ((prop (pop plist))
                          (sym  (pop plist)))
                      (when-let* ((icon (plist-get +pretty-code-symbols prop)))
                        (push (cons sym (prettify-utils-string icon))
                              results))))
                  (setq prettify-symbols-alist results))
                (prettify-symbols-mode -1)
                (prettify-symbols-mode +1))))
      (add-hook (intern (format "%s-hook" mode)) fn))))
