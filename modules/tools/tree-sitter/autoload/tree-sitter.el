;;; tools/tree-sitter/autoload/tree-sitter.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'tree-sitter! #'ignore)
(defun tree-sitter! ()
  (message "Old tree-sitter.el support is deprecated!"))

;;;###autodef (fset 'set-tree-sitter! #'ignore)
(defun set-tree-sitter! (modes ts-mode &optional recipes)
  "Remap major MODES to TS-MODE.

MODES and TS-MODE are major mode symbols. MODES can be a list thereof. If
RECIPES is provided, fall back to MODES if RECIPES don't pass `treesit-ready-p'
when activating TS-MODE. Use this for ts modes that error out instead of failing
gracefully.

RECIPES is a symbol (a grammar language name), list thereof, or alist of plists
with the format (LANG &key URL REV SOURCE-DIR CC CPP COMMIT). If an alist of
plists, it will be transformed into entries for `treesit-language-source-alist'
(which describe what each of these keys mean). Note that COMMIT is ignored
pre-Emacs 31."
  (declare (indent 2))
  (cl-check-type modes (or list symbol))
  (cl-check-type ts-mode symbol)
  (let ((recipes (mapcar #'ensure-list (ensure-list recipes)))
        (modes (ensure-list modes)))
    (when modes
      ;; Most ts-modes do not register their base modes as parents until Emacs
      ;; 30/31; this backports that.
      (put ts-mode 'derived-mode-extra-parents modes))
    (dolist (m (or modes (list nil)))
      (when m
        (setf (alist-get m major-mode-remap-defaults) ts-mode))
      (put ts-mode '+tree-sitter (cons m (mapcar #'car recipes))))
    (when-let* ((fn (intern-soft (format "%s-maybe" ts-mode))))
      (cl-callf2 rassq-delete-all fn auto-mode-alist)
      (cl-callf2 rassq-delete-all fn interpreter-mode-alist))
    (when-let* ((recipes (cl-delete-if-not #'cdr recipes)))
      (with-eval-after-load 'treesit
        (dolist (recipe recipes)
          (cl-destructuring-bind (name &key url rev source-dir cc cpp commit) (ensure-list recipe)
            (setf (alist-get name treesit-language-source-alist)
                  (append (list url rev source-dir cc cpp)
                          ;; COMPAT: 31.1 introduced a COMMIT recipe argument. On
                          ;;   <=30.x, extra arguments will trigger an arity error
                          ;;   when installing grammars.
                          (if (eq (cdr (func-arity
                                        (advice--cd*r
                                         (advice--symbol-function 'treesit--install-language-grammar-1))))
                                  'many)
                              (list commit))))))))))

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

;;;###autoload
(defun +tree-sitter/doctor ()
  "Test if the current buffer is correctly tree-sitter-enabled."
  (interactive)
  (unless (treesit-available-p)
    (user-error "This Emacs install wasn't built with treesit support!"))
  (unless (treesit-parser-list)
    (user-error "No tree-sitter parsers are active in this buffer; are the grammars properly installed?"))
  (message "Tree-sitter is functioning in this buffer!"))

;;; tree-sitter.el ends here
