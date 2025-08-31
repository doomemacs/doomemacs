;;; tools/tree-sitter/autoload/tree-sitter.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'tree-sitter! #'ignore)
(defun tree-sitter! ()
  (message "Old tree-sitter.el support is deprecated!"))

;;;###autodef (fset 'set-tree-sitter! #'ignore)
(defun set-tree-sitter! (mode ts-mode &optional recipes)
  "Remap major MODE to TS-MODE.

MODE and TS-MODE are major mode symbols. If RECIPES is provided, fall back to
MODE if RECIPES don't pass `treesit-ready-p' when activating TS-MODE. Use this
for ts modes that error out instead of failing gracefully.

RECIPES is a symbol (a grammar language name), list thereof, or alist of plists
with the format (LANG &key URL REV SOURCE-DIR CC CPP COMMIT). If an alist of
plists, it will be transformed into entries for `treesit-language-source-alist'
(which describe what each of these keys mean). Note that COMMIT is ignored
pre-Emacs 31."
  (declare (indent 2))
  (cl-check-type mode symbol)
  (cl-check-type ts-mode symbol)
  (setq recipes (mapcar #'ensure-list (ensure-list recipes)))
  (dolist (m (ensure-list mode))
    (setf (alist-get m major-mode-remap-defaults) ts-mode)
    (put ts-mode '+tree-sitter (cons m (mapcar #'car recipes))))
  (when (setq recipes (cl-remove-if-not #'cdr recipes))
    (with-eval-after-load 'treesit
      (dolist (recipe recipes)
        (cl-destructuring-bind (name &key url rev source-dir cc cpp commit) (ensure-list recipe)
          (setf (alist-get name treesit-language-source-alist)
                (append (list url rev source-dir cc cpp)
                        ;; COMPAT: 31.1 introduced a COMMIT recipe argument. On
                        ;;   <=30.x, extra arguments will trigger an arity error
                        ;;   when installing grammars.
                        (if (eq (cdr (func-arity 'treesit--install-language-grammar-1))
                                'many)
                            (list commit)))))))))

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

;;;###autoload
(defun +tree-sitter-ts-mode-inhibit-side-effects-a (fn &rest args)
  "Suppress changes to `auto-mode-alist' and `interpreter-mode-alist'."
  (let (auto-mode-alist interpreter-mode-alist)
    (apply fn args)))

;;; TODO: Backwards compatibility

;;; tree-sitter.el ends here
