;;; module-asm.el

(use-package asm-mode
  :commands (mips-mode)
  :mode ("\\.mips" . mips-mode)
  :config
  (define-derived-mode mips-mode asm-mode "MIPS"
    "Major mode for editing MIPS assembler code."
    ;; Unset ; key.
    (local-unset-key (vector asm-comment-char))
    (set (make-local-variable #'asm-comment-char) ?#)
    (local-set-key (vector asm-comment-char) #'asm-comment)
    ;; Update syntax for new comment char.
    (set-syntax-table (make-syntax-table asm-mode-syntax-table))
    (modify-syntax-entry asm-comment-char "< b")
    ;; Fix one level comments.
    (set (make-local-variable #'comment-start) (string asm-comment-char))))

(provide 'module-asm)
;;; module-asm.el ends here
