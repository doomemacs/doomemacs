;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(defvar +cc-default-include-paths
  (list "include"
        "includes")
  "A list of default relative paths which will be searched for up from the
current file. Paths can be absolute. This is ignored if your project has a
compilation database.

This is ignored by ccls.")

(defvar +cc-default-header-file-mode 'c-mode
  "Fallback major mode for .h files if all other heuristics fail (in
`+cc-c-c++-objc-mode').")


;;
;;; Packages

(use-package! cc-mode
  :mode ("\\.mm\\'" . objc-mode)
  ;; Use `c-mode'/`c++-mode'/`objc-mode' depending on heuristics
  :mode ("\\.h\\'" . +cc-c-c++-objc-mode)
  ;; Ensure find-file-at-point recognize system libraries in C modes. It must be
  ;; set up before lsp is initialized. Also, we use local-vars hooks to ensure
  ;; these only run in their respective major modes, and not derived modes.
  :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars) . +cc-init-ffap-integration-h)
  :hook ((c-ts-mode-local-vars c++-ts-mode-local-vars) . +cc-init-ffap-integration-h)
  ;;; Improve fontification in C/C++ (also see `modern-cpp-font-lock')
  :init
  (when (modulep! +tree-sitter)
    (set-tree-sitter! 'c-mode 'c-ts-mode
      '((c :url "https://github.com/tree-sitter/tree-sitter-c")))
    (set-tree-sitter! 'c++-mode 'c++-ts-mode
      '((cpp :url "https://github.com/tree-sitter/tree-sitter-cpp"))))
  :config
  (set-docsets! '(c-mode c-ts-mode) "C")
  (set-docsets! '(c++-mode c++-ts-mode) "C++" "Boost")
  (set-electric! '(c-mode c++-mode objc-mode java-mode
                   c-ts-mode c++-ts-mode java-ts-mode)
                 :chars '(?\n ?\} ?\{))
  (set-rotate-patterns! '(c++-mode c++-ts-mode)
    :symbols '(("public" "protected" "private")
               ("class" "struct")))
  (set-ligatures! '(c-mode c-ts-mode c++-mode c++-ts-mode)
    ;; Functional
    ;; :def "void "
    ;; Types
    :null "nullptr"
    :true "true" :false "false"
    :int "int" :float "float"
    :str "std::string"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :return "return"
    :yield "#require")

  (add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.c\\(c\\|pp\\)?\\'" "\\1.h\\(h\\|pp\\)?\\'"))
  (add-to-list 'find-sibling-rules '("/\\([^/]+\\)\\.h\\(h\\|pp\\)?\\'" "\\1.c\\(c\\|pp\\)?\\'"))

  ;; HACK Suppress 'Args out of range' error in when multiple modifications are
  ;;      performed at once in a `c++-mode' buffer, e.g. with `iedit' or
  ;;      multiple cursors.
  (undefadvice! +cc--suppress-silly-errors-a (fn &rest args)
    :around #'c-after-change-mark-abnormal-strings
    (ignore-errors (apply fn args)))

  ;; Custom style, based off of linux
  (setq c-basic-offset tab-width
        c-backspace-function #'delete-backward-char)

  (c-add-style
   "doom" '((c-comment-only-line-offset . 0)
            (c-hanging-braces-alist (brace-list-open)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty))
            (c-cleanup-list brace-else-brace)
            (c-offsets-alist
             (knr-argdecl-intro . 0)
             (substatement-open . 0)
             (substatement-label . 0)
             (statement-cont . +)
             (case-label . +)
             ;; align args with open brace OR don't indent at all (if open
             ;; brace is at eolp and close brace is after arg with no trailing
             ;; comma)
             (brace-list-intro . 0)
             (brace-list-close . -)
             (arglist-intro . +)
             (arglist-close +cc-lineup-arglist-close 0)
             ;; don't over-indent lambda blocks
             (inline-open . 0)
             (inlambda . 0)
             ;; indent access keywords +1 level, and properties beneath them
             ;; another level
             (access-label . -)
             (inclass +cc-c++-lineup-inclass +)
             (label . 0))))

  (when (listp c-default-style)
    (setf (alist-get 'other c-default-style) "doom")))


;;
;; Major modes

(use-package! cmake-mode
  :defer t
  :init
  (when (and (modulep! +tree-sitter)
             (boundp 'cmake-ts-mode)) ; 29+ only
    (set-tree-sitter! 'cmake-mode 'cmake-ts-mode
      '((cmake :url "https://github.com/uyha/tree-sitter-cmake"))))
  :config
  (set-docsets! '(cmake-mode cmake-ts-mode) "CMake")
  (set-popup-rule! "^\\*CMake Help\\*" :size 0.4 :ttl t)
  (set-lookup-handlers! '(cmake-mode cmake-ts-mode)
    :documentation '+cc-cmake-lookup-documentation-fn)
  (when (require 'company-cmake nil t)
    (set-company-backend! '(cmake-mode cmake-ts-mode) 'company-cmake))
  (when (modulep! +lsp)
    (add-hook 'cmake-mode-local-vars-hook #'lsp! 'append)
    (add-hook 'cmake-ts-mode-local-vars-hook #'lsp! 'append)))


(use-package! glsl-mode
  :defer t
  :init
  (when (modulep! +tree-sitter)
    (set-tree-sitter! 'glsl-mode 'glsl-ts-mode
      '((glsl :url "https://github.com/tree-sitter-grammars/tree-sitter-glsl"))))
  :config
  (when (require 'company-glsl nil t)
    (set-company-backend! 'glsl-mode 'company-glsl))
  (when (modulep! +lsp)
    (add-hook 'glsl-mode-local-vars-hook #'lsp! 'append)
    (add-hook 'glsl-ts-mode-local-vars-hook #'lsp! 'append)))


(use-package! cuda-mode
  :defer t
  :config
  (when (modulep! +lsp)
    (add-hook 'cuda-mode-local-vars-hook #'lsp! 'append))
  )


(use-package! cuda-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'cuda-mode 'cuda-ts-mode
    '((cuda :url "https://github.com/tree-sitter-grammars/tree-sitter-cuda")))
  :config
  (when (modulep! +lsp)
    (add-hook 'cuda-ts-mode-local-vars-hook #'lsp! 'append)))


(use-package! demangle-mode
  :hook llvm-mode)


;;
;;; LSP

(when (modulep! +lsp)
  (add-hook! '(c-mode-local-vars-hook
               c-ts-mode-local-vars-hook
               c++-mode-local-vars-hook
               c++-ts-mode-local-vars-hook
               objc-mode-local-vars-hook)
             :append #'lsp!)

  (if (modulep! :tools lsp -eglot)
      (after! lsp-clangd
        ;; Prevent clangd from consuming all your cores indexing larger projects
        ;; and grinding your system to a halt.
        (cl-pushnew (format "-j=%d" (max 1 (/ (doom-system-cpus) 2)))
                    lsp-clients-clangd-args))
    (set-eglot-client! 'cuda-mode '("clangd"))

    ;; Map eglot specific helper
    (map! :localleader
          :after cc-mode
          :map c++-mode-map
          :desc "Show type inheritance hierarchy" "ct" #'+cc/eglot-ccls-inheritance-hierarchy)

    ;; NOTE : This setting is untested yet
    (after! eglot
      (when (featurep :system 'macos)
        (add-to-list 'eglot-workspace-configuration
                     `((:ccls . ((:clang . ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                                              "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                                              "-isystem/usr/local/include"]
                                                  :resourceDir (cdr (doom-call-process "clang" "-print-resource-dir"))))))))))))

(use-package! ccls
  :when (modulep! +lsp)
  :when (modulep! :tools lsp -eglot)
  :defer t
  :init
  (defvar ccls-sem-highlight-method 'font-lock)
  (after! project
    (add-to-list 'project-vc-ignores "^\\.ccls-cache$"))
  ;; DEPRECATED: Remove when projectile is replaced with project.el
  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
  :config
  (set-evil-initial-state! 'ccls-tree-mode 'emacs)
  (set-lsp-priority! 'ccls -2) ; Prioritize clangd over ccls
  ;; Disable `ccls-sem-highlight-method' if `lsp-enable-semantic-highlighting'
  ;; is nil. Otherwise, it appears ccls bypasses it.
  (setq-hook! 'lsp-configure-hook
    ccls-sem-highlight-method (if lsp-enable-semantic-highlighting
                                  ccls-sem-highlight-method))
  (when (or (featurep :system 'macos)
            (featurep :system 'linux))
    (setq ccls-initialization-options
          `(:index (:trackDependency 1
                    ;; Prevent ccls from consuming all your cores indexing
                    ;; larger projects and grinding your system to a halt.
                    :threads ,(max 1 (/ (doom-system-cpus) 2))))))
  (when (featurep :system 'macos)
    (setq ccls-initialization-options
          (append ccls-initialization-options
                  `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                              "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                              "-isystem/usr/local/include"]
                                  :resourceDir (cdr (doom-call-process "clang" "-print-resource-dir")))))))
  (map! :after cc-mode
        :map (c-mode-map c++-mode-map)
        :n "C-h" (cmd! (ccls-navigate "U"))
        :n "C-j" (cmd! (ccls-navigate "R"))
        :n "C-k" (cmd! (ccls-navigate "L"))
        :n "C-l" (cmd! (ccls-navigate "D"))
        (:localleader
         :desc "Preprocess file"        "lp" #'ccls-preprocess-file
         :desc "Reload cache & CCLS"    "lf" #'ccls-reload)
        (:when (modulep! :tools lsp +peek)
         (:localleader
          :desc "Callers list"          "c" #'+cc/ccls-show-caller
          :desc "Callees list"          "C" #'+cc/ccls-show-callee
          :desc "References (address)"  "a" #'+cc/ccls-show-references-address
          :desc "References (not call)" "f" #'+cc/ccls-show-references-not-call
          :desc "References (Macro)"    "m" #'+cc/ccls-show-references-macro
          :desc "References (Read)"     "r" #'+cc/ccls-show-references-read
          :desc "References (Write)"    "w" #'+cc/ccls-show-references-write))))
