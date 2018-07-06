;;; ui/pretty-code/settings.el -*- lexical-binding: t; -*-

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
    :map           "↦"
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
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•")
  "Options plist for `set-pretty-symbols!'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

;;;###autoload
(defvar +pretty-code-symbols-alist nil
  "An alist containing a mapping of major modes to its value for
`prettify-symbols-alist'.")


;;;###autoload
(defun set-pretty-symbols! (modes &rest rest)
  (dolist (mode (doom-enlist modes))
    (if (null (car-safe rest))
        (setq var nil)
      (let (results merge key alist)
        (while rest
          (setq key (pop rest))
          (pcase key
            (:merge (setq merge t))
            (:alist (setq results (append (pop rest) results)))
            (_
             (unless (plist-member +pretty-code-symbols key)
               (user-error "Invalid keyword in set-pretty-symbols!: %s" key))
             (let* ((sym (pop rest))
                    (char (plist-get +pretty-code-symbols key)))
               (push (cons sym char) results)))))
        (unless merge
          (assq-delete-all mode +pretty-code-symbols-alist))
        (push (cons mode results) +pretty-code-symbols-alist)))))
