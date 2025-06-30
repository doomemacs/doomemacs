;;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

(defvar +tree-sitter--major-mode-remaps-alist nil)


;;
;;; Packages

(use-package! treesit
  :when (fboundp 'treesit-available-p)
  :when (treesit-available-p)
  :defer t
  :config
  ;; HACK: treesit lacks any way to dictate where to install grammars.
  (add-to-list 'treesit-extra-load-path (concat doom-profile-data-dir "tree-sitter"))
  (defadvice! +tree-sitter--install-grammar-to-local-dir-a (fn &rest args)
    "Write grammars to `doom-profile-data-dir'."
    :around #'treesit-install-language-grammar
    :around #'treesit--build-grammar
    (let ((user-emacs-directory doom-profile-data-dir))
      (apply fn args)))

  ;; HACK: Some *-ts-mode packages modify `major-mode-remap-defaults'
  ;;   inconsistently. Playing whack-a-mole to undo those changes is more hassle
  ;;   then simply ignoring them (by overriding `major-mode-remap-defaults' for
  ;;   any modes remapped with `set-tree-sitter!'). The user shouldn't touch
  ;;   `major-mode-remap-defaults' anyway; `major-mode-remap-alist' will always
  ;;   have precedence.
  (defadvice! +tree-sitter--ignore-default-major-mode-remaps-a (fn mode)
    :around #'major-mode-remap
    (let ((major-mode-remap-defaults
           (if-let* ((m (assq mode +tree-sitter--major-mode-remaps-alist)))
               +tree-sitter--major-mode-remaps-alist
             major-mode-remap-defaults)))
      (funcall fn mode)))

  ;; TODO: Move most of these out to modules
  (dolist (map '((awk "https://github.com/Beaglefoot/tree-sitter-awk" nil nil nil nil)
                 (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex" nil nil nil nil)
                 (blueprint "https://github.com/huanie/tree-sitter-blueprint" nil nil nil nil)
                 (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" nil nil nil nil)
                 (clojure "https://github.com/sogaiu/tree-sitter-clojure" nil nil nil nil)
                 (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp" nil nil nil nil)
                 (css "https://github.com/tree-sitter/tree-sitter-css" nil nil nil nil)
                 (html "https://github.com/tree-sitter/tree-sitter-html" nil nil nil nil)
                 (java "https://github.com/tree-sitter/tree-sitter-java" nil nil nil nil)
                 (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src" nil nil)
                 (latex "https://github.com/latex-lsp/tree-sitter-latex" nil nil nil nil)
                 (make "https://github.com/tree-sitter-grammars/tree-sitter-make" nil nil nil nil)
                 (nu "https://github.com/nushell/tree-sitter-nu" nil nil nil nil)
                 (org "https://github.com/milisims/tree-sitter-org" nil nil nil nil)
                 (perl "https://github.com/ganezdragon/tree-sitter-perl" nil nil nil nil)
                 (proto "https://github.com/mitchellh/tree-sitter-proto" nil nil nil nil)
                 (r "https://github.com/r-lib/tree-sitter-r" nil nil nil nil)
                 (rust "https://github.com/tree-sitter/tree-sitter-rust" nil nil nil nil)
                 (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages" nil nil nil)
                 (surface "https://github.com/connorlay/tree-sitter-surface" nil nil nil nil)
                 (toml "https://github.com/tree-sitter/tree-sitter-toml" nil nil nil nil)
                 (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil)
                 (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
                 (typst "https://github.com/uben0/tree-sitter-typst" "master" "src" nil nil)
                 (verilog "https://github.com/gmlarumbe/tree-sitter-verilog" nil nil nil nil)
                 (vhdl "https://github.com/alemuller/tree-sitter-vhdl" nil nil nil nil)
                 (vue "https://github.com/tree-sitter-grammars/tree-sitter-vue" nil nil nil nil)
                 (wast "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src" nil nil)
                 (wat "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src" nil nil)
                 (wgsl "https://github.com/mehmetoguzderin/tree-sitter-wgsl" nil nil nil nil)))
    (cl-pushnew map treesit-language-source-alist :test #'eq :key #'car)))


;; TODO: combobulate or evil-textobj-tree-sitter


;; (use-package! combobulate
;;   :commands combobulate-query-builder
;;   :hook (prog-mode . combobulate-mode))


;; (use-package! evil-textobj-tree-sitter
;;   :when (modulep! :editor evil +everywhere)
;;   :defer t
;;   :init (after! tree-sitter (require 'evil-textobj-tree-sitter))
;;   :after-call doom-first-input-hook
;;   :config
;;   (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
;;   (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
;;   (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
;;   (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

;;   (evil-define-key '(visual operator) 'tree-sitter-mode
;;     "i" +tree-sitter-inner-text-objects-map
;;     "a" +tree-sitter-outer-text-objects-map)
;;   (evil-define-key 'normal 'tree-sitter-mode
;;     "[g" +tree-sitter-goto-previous-map
;;     "]g" +tree-sitter-goto-next-map)

;;   (map! (:map +tree-sitter-inner-text-objects-map
;;          "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
;;          "f" (+tree-sitter-get-textobj "function.inner")
;;          "F" (+tree-sitter-get-textobj "call.inner")
;;          "C" (+tree-sitter-get-textobj "class.inner")
;;          "v" (+tree-sitter-get-textobj "conditional.inner")
;;          "l" (+tree-sitter-get-textobj "loop.inner"))
;;         (:map +tree-sitter-outer-text-objects-map
;;          "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
;;          "f" (+tree-sitter-get-textobj "function.outer")
;;          "F" (+tree-sitter-get-textobj "call.outer")
;;          "C" (+tree-sitter-get-textobj "class.outer")
;;          "c" (+tree-sitter-get-textobj "comment.outer")
;;          "v" (+tree-sitter-get-textobj "conditional.outer")
;;          "l" (+tree-sitter-get-textobj "loop.outer"))

;;         (:map +tree-sitter-goto-previous-map
;;          "a" (+tree-sitter-goto-textobj "parameter.outer" t)
;;          "f" (+tree-sitter-goto-textobj "function.outer" t)
;;          "F" (+tree-sitter-goto-textobj "call.outer" t)
;;          "C" (+tree-sitter-goto-textobj "class.outer" t)
;;          "c" (+tree-sitter-goto-textobj "comment.outer" t)
;;          "v" (+tree-sitter-goto-textobj "conditional.outer" t)
;;          "l" (+tree-sitter-goto-textobj "loop.outer" t))
;;         (:map +tree-sitter-goto-next-map
;;          "a" (+tree-sitter-goto-textobj "parameter.outer")
;;          "f" (+tree-sitter-goto-textobj "function.outer")
;;          "F" (+tree-sitter-goto-textobj "call.outer")
;;          "C" (+tree-sitter-goto-textobj "class.outer")
;;          "c" (+tree-sitter-goto-textobj "comment.outer")
;;          "v" (+tree-sitter-goto-textobj "conditional.outer")
;;          "l" (+tree-sitter-goto-textobj "loop.outer")))

;;   (after! which-key
;;     (setq which-key-allow-multiple-replacements t)
;;     (pushnew!
;;      which-key-replacement-alist
;;      '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1")))))
