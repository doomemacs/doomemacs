;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(defvar +cc-default-include-paths
  (list "include"
        "includes")
  "A list of default relative paths which will be searched for up from the
current file, to be passed to irony as extra header search paths. Paths can be
absolute. This is ignored if your project has a compilation database.

This is ignored by ccls.")

(defvar +cc-default-header-file-mode 'c-mode
  "Fallback major mode for .h files if all other heuristics fail (in
`+cc-c-c++-objc-mode').")

(defvar +cc-default-compiler-options
  `((c-mode . nil)
    (c++-mode
     . ,(list "-std=c++1z" ; use C++17 draft by default
              (when IS-MAC
                ;; NOTE beware: you'll get abi-inconsistencies when passing
                ;; std-objects to libraries linked with libstdc++ (e.g. if you
                ;; use boost which wasn't compiled with libc++)
                "-stdlib=libc++")))
    (objc-mode . nil))
  "A list of default compiler options for the C family. These are ignored if a
compilation database is present in the project.

This is ignored by ccls.")


;;
;; Packages

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm\\'" . objc-mode)
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "doom")

  ;; The plusses in c++-mode can be annoying to search for ivy/helm (which reads
  ;; queries as regexps), so we add these for convenience.
  (defalias 'cpp-mode 'c++-mode)
  (defvaralias 'cpp-mode-map 'c++-mode-map)

  ;; Activate `c-mode', `c++-mode' or `objc-mode' depending on heuristics
  (add-to-list 'auto-mode-alist '("\\.h\\'" . +cc-c-c++-objc-mode))

  ;; Ensure find-file-at-point works in C modes, must be added before irony
  ;; and/or lsp hooks are run.
  (add-hook! (c-mode-local-vars c++-mode-local-vars objc-mode-local-vars)
    #'+cc|init-ffap-integration)

  :config
  (set-electric! '(c-mode c++-mode objc-mode java-mode) :chars '(?\n ?\} ?\{))
  (set-docsets! 'c-mode "C")
  (set-docsets! 'c++-mode "C++" "Boost")

  (set-rotate-patterns! 'c++-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))

  (set-pretty-symbols! '(c-mode c++-mode)
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

  ;;; Better fontification (also see `modern-cpp-font-lock')
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)

  ;; Custom style, based off of linux
  (c-add-style
   "doom" '((c-basic-offset . tab-width)
            (c-comment-only-line-offset . 0)
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

  ;;; Keybindings
  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (define-key! c++-mode-map "<" nil ">" nil)
  ;; ...and leave it to smartparens
  (sp-with-modes '(c++-mode objc-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))

  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))


(def-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))


(def-package! irony
  :unless (featurep! +lsp)
  :commands (irony-install-server irony-mode)
  :preface
  (setq irony-server-install-prefix (concat doom-etc-dir "irony-server/"))
  :init
  (defun +cc|init-irony-mode ()
    (if (file-directory-p irony-server-install-prefix)
        (irony-mode +1)
      (message "Irony server isn't installed")))
  (add-hook! (c-mode-local-vars c++-mode-local-vars objc-mode-local-vars)
    #'+cc|init-irony-mode)
  :config
  (setq irony-cdb-search-directory-list '("." "build" "build-conda"))

  ;; Initialize compilation database, if present. Otherwise, fall back on
  ;; `+cc-default-compiler-options'.
  (add-hook 'irony-mode-hook #'+cc|init-irony-compile-options)

  (def-package! irony-eldoc
    :hook (irony-mode . irony-eldoc))

  (def-package! flycheck-irony
    :when (featurep! :tools flycheck)
    :config (flycheck-irony-setup))

  (def-package! company-irony
    :when (featurep! :completion company)
    :init
    (set-company-backend! 'irony-mode
      '(:separate company-irony-c-headers company-irony))
    :config
    (require 'company-irony-c-headers)))


;;
;; Major modes

(def-package! company-cmake  ; for `cmake-mode'
  :when (featurep! :completion company)
  :after cmake-mode
  :config (set-company-backend! 'cmake-mode 'company-cmake))


(def-package! demangle-mode
  :hook llvm-mode)


(def-package! company-glsl  ; for `glsl-mode'
  :when (featurep! :completion company)
  :after glsl-mode
  :config (set-company-backend! 'glsl-mode 'company-glsl))


;;
;; Rtags Support

(def-package! rtags
  :unless (featurep! +lsp)
  :commands rtags-executable-find
  :preface
  (setq rtags-install-path (concat doom-etc-dir "rtags/"))
  :init
  (defun +cc|init-rtags ()
    "Start an rtags server in c-mode and c++-mode buffers."
    (when (and (require 'rtags nil t)
               (rtags-executable-find rtags-rdm-binary-name))
      (rtags-start-process-unless-running)))
  (add-hook! (c-mode-local-vars c++-mode-local-vars objc-mode-local-vars)
    #'+cc|init-rtags)
  :config
  (setq rtags-autostart-diagnostics t
        rtags-use-bookmarks nil
        rtags-completions-enabled nil
        rtags-display-result-backend
        (cond ((featurep! :completion ivy)  'ivy)
              ((featurep! :completion helm) 'helm)
              ('default))
        ;; If not using ivy or helm to view results, use a pop-up window rather
        ;; than displaying it in the current window...
        rtags-results-buffer-other-window t
        ;; ...and don't auto-jump to first match before making a selection.
        rtags-jump-to-first-match nil)

  (set-lookup-handlers! '(c-mode c++-mode)
    :definition #'rtags-find-symbol-at-point
    :references #'rtags-find-references-at-point)

  (add-hook! 'kill-emacs-hook (ignore-errors (rtags-cancel-process)))

  ;; Use rtags-imenu instead of imenu/counsel-imenu
  (define-key! (c-mode-map c++-mode-map) [remap imenu] #'+cc/imenu)

  (when (featurep 'evil)
    (add-hook 'rtags-jump-hook #'evil-set-jump))
  (add-hook 'rtags-after-find-file-hook #'recenter))


;;
;; LSP

(def-package! ccls
  :when (featurep! +lsp)
  :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars) . +cc|init-ccls)
  :init
  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
    (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
  :config
  (defun +cc|init-ccls ()
    (setq-local company-transformers nil)
    (setq-local company-lsp-async t)
    (setq-local company-lsp-cache-candidates nil)
    (lsp)))
