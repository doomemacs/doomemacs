;;; tools/electric-indent/config.el

;; Smarter, keyword-based electric-indent

(defvar doom-electric-indent-p nil
  "TODO")

(defvar-local doom-electric-indent-words '()
  "TODO")

(setq electric-indent-chars '(?\n ?\^?))

(push (lambda (c)
        (when (and (eolp) doom-electric-indent-words)
          (save-excursion
            (backward-word)
            (looking-at-p
             (concat "\\<" (regexp-opt doom-electric-indent-words))))))
      electric-indent-functions)

(@def-setting :electric (modes &rest plist)
  "Declare :words (list of strings) or :chars (lists of chars) in MODES that
trigger electric indentation."
  (declare (indent 1))
  (let ((modes (if (listp modes) modes (list modes)))
        (chars (doom-mplist-get plist :chars))
        (words (doom-mplist-get plist :words)))
    (when (or chars words)
      (let ((fn-name (intern (format "doom--electric-%s" (s-join "-" (mapcar 'symbol-name modes))))))
        `(progn
           (defun ,fn-name ()
             (electric-indent-local-mode +1)
             ,(if chars `(setq electric-indent-chars ',chars))
             ,(if words `(setq doom-electric-indent-words ',words)))
           (@add-hook ,modes ',fn-name))))))

