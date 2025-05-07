;;; tools/tree-sitter/autoload/tree-sitter.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'tree-sitter! #'ignore)
(defun tree-sitter! ()
  (message "Old tree-sitter.el support is deprecated!"))

;;;###autodef (fset 'set-tree-sitter! #'ignore)
(defun set-tree-sitter! (mode ts-mode &optional langs)
  "Remap major MODE to TS-MODE.

MODE and TS-MODE are major mode symbols. If LANGS is provided, fall back to MODE
if LANGS don't pass `treesit-ready-p' when activating TS-MODE. Use this for ts
modes that error out instead of failing gracefully."
  (declare (indent 2))
  (cl-check-type mode symbol)
  (cl-check-type ts-mode symbol)
  (setq langs (ensure-list langs))
  (dolist (m (ensure-list mode))
    (add-to-list
     'major-mode-remap-defaults
     (cons
      m (let (ensured?)
          (lambda ()
            (funcall
             ;; Because standard major-mode remapping doesn't offer graceful
             ;; failure in some cases, I implement it myself:
             (cond ((null langs) m)
                   ((not (fboundp ts-mode))
                    (message "Couldn't find %S, using %S instead" ts-mode m)
                    m)
                   ((and (fboundp 'treesit-available-p)
                         (treesit-available-p)
                         (fboundp ts-mode)
                         ;; Only prompt once, and log other times.
                         (cl-every (if ensured?
                                       (doom-rpartial #'treesit-ready-p 'message)
                                     #'treesit-ensure-installed)
                                   (cl-loop for lang in langs
                                            if (listp lang)
                                            collect (car lang)
                                            else collect (list lang))))
                    ts-mode)
                   ((setq ensured? t)
                    m))))))))
  (with-eval-after-load 'treesit
    (dolist (lang langs)
      (when (and lang (listp lang))
        (cl-destructuring-bind (name &key url rev source-dir cc cpp commit) lang
          (setf (alist-get name treesit-language-source-alist)
                (list url rev source-dir cc cpp commit)))))))

;; ;; HACK: Remove and refactor when `use-package' eager macro expansion is solved or `use-package!' is removed
;; ;;;###autoload
;; (defun +tree-sitter-get-textobj (group &optional query)
;;   "A wrapper around `evil-textobj-tree-sitter-get-textobj' to
;; prevent eager expansion."
;;   (eval `(evil-textobj-tree-sitter-get-textobj ,group ,query)))

;; ;;;###autoload
;; (defun +tree-sitter-goto-textobj (group &optional previous end query)
;;   "Thin wrapper that returns the symbol of a named function, used in keybindings."
;;   (let ((sym (intern (format "+goto%s%s-%s" (if previous "-previous" "") (if end "-end" "") group))))
;;     (fset sym (lambda ()
;;                 (interactive)
;;                 (evil-textobj-tree-sitter-goto-textobj group previous end query)))
;;     sym))


;;; TODO: Backwards compatibility

;;; tree-sitter.el ends here
