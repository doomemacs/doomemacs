;;; tools/electric-indent/config.el -*- lexical-binding: t; -*-

;; Smarter, keyword-based electric-indent

(defvar doom-electric-indent-p nil
  "TODO")

(defvar-local doom-electric-indent-words '()
  "TODO")

(setq-default electric-indent-chars '(?\n ?\^?))

(defun +electric-indent|char (_c)
  (when (and (eolp) doom-electric-indent-words)
    (save-excursion
      (backward-word)
      (looking-at-p
       (concat "\\<" (regexp-opt doom-electric-indent-words))))))
(cl-pushnew #'+electric-indent|char electric-indent-functions :test #'eq)

(def-setting! :electric (modes &rest plist)
  "Declare :words (list of strings) or :chars (lists of chars) in MODES that
trigger electric indentation."
  (declare (indent 1))
  (let ((modes (doom-enlist (doom-unquote modes)))
        (chars (doom-unquote (plist-get plist :chars)))
        (words (doom-unquote (plist-get plist :words))))
    (when (or chars words)
      (let ((fn-name (intern (format "doom--init-electric-%s" (mapconcat #'symbol-name modes "-")))))
        `(progn
           (defun ,fn-name ()
             (electric-indent-local-mode +1)
             ,@(if chars `((setq electric-indent-chars ',chars)))
             ,@(if words `((setq doom-electric-indent-words ',words))))
           (add-hook! ,modes #',fn-name))))))

