;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm" . objc-mode)
  :init
  (setq-default c-basic-offset tab-width)

  (defun +cc--c++-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (or (file-exists-p (expand-file-name
                             (concat (file-name-sans-extension buffer-file-name)
                                     ".cpp")))
             (when-let (file (car-safe (projectile-get-other-files
                                        buffer-file-name
                                        (projectile-current-project-files))))
               (equal (file-name-extension file) "cpp")))))

  (defun +cc--objc-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))

  ;; Auto-detect C++/Obj-C header files
  (push (cons #'+cc--c++-header-file-p  'c++-mode)  magic-mode-alist)
  (push (cons #'+cc--objc-header-file-p 'objc-mode) magic-mode-alist)

  :config
  (setq c-tab-always-indent nil
        c-electric-flag nil)

  (set! :electric '(c-mode c++-mode objc-mode java-mode)
        :chars '(?\n ?\}))

  (set! :company-backend
        '(c-mode c++-mode objc-mode)
        '(company-irony-c-headers company-irony))

  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook 'c-mode-hook #'highlight-numbers-mode) ; fontify numbers in C
  (add-hook 'c++-mode-hook #'+cc|extra-fontify-c++) ; fontify C++11 string literals

  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "<" ">" :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p))
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Improve indentation of inline lambdas in C++11
  (advice-add #'c-lineup-arglist :around #'+c-lineup-arglist)

  ;; C/C++ style settings
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+)        ; indent case labels by c-indent-level, too
  (c-set-offset 'access-label '-)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)

  (defun +cc--c-lineup-inclass (_langelem)
    (if (memq major-mode '(c-mode c++-mode))
        (let ((inclass (assq 'inclass c-syntactic-context)))
          (save-excursion
            (goto-char (c-langelem-pos inclass))
            (if (or (looking-at "struct")
                    (looking-at "typedef struct"))
                '+
              '++)))
      '+))
  (c-set-offset 'inclass #'+cc--c-lineup-inclass)


  ;; Certain mappings interfere with smartparens and custom bindings,
  ;; so unbind them
  (map! :map c-mode-map
        "DEL" nil
        "#" #'self-insert-command
        "{" #'self-insert-command
        "}" #'self-insert-command
        "/" #'self-insert-command
        "*" #'self-insert-command
        ";" #'self-insert-command
        "," #'self-insert-command
        ":" #'self-insert-command
        "(" #'self-insert-command
        ")" #'self-insert-command

        :map c++-mode-map
        "}" nil

        ;; Smartparens and cc-mode both try to autoclose angle-brackets
        ;; intelligently. The result isn't very intelligent (causes redundant
        ;; characters), so just do it ourselves.
        "<" nil
        :map (c-mode-base-map c++-mode-map)
        :i ">" #'+cc/autoclose->-maybe))


(def-package! modern-cpp-font-lock
  :commands modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


(def-package! irony
  :after cc-mode
  :commands irony-install-server
  :preface
  (setq irony-server-install-prefix (concat doom-etc-dir "irony-server/"))
  :init
  (defun +cc|init-irony-mode ()
    (when (and (memq major-mode '(c-mode c++-mode objc-mode))
               (file-directory-p irony-server-install-prefix))
      (irony-mode +1)))
  (add-hook 'c-mode-common-hook #'+cc|init-irony-mode)
  :config
  (add-hook! 'irony-mode-hook #'(irony-eldoc flycheck-mode))

  (defun +cc|init-c++11-clang-options ()
    (make-local-variable 'irony-additional-clang-options)
    (cl-pushnew "-std=c++11" irony-additional-clang-options :test 'equal))
  (add-hook 'c++-mode-hook #'+cc|init-c++11-clang-options)

  (map! :map irony-mode-map
        [remap completion-at-point] #'counsel-irony
        [remap complete-symbol] #'counsel-irony))

(def-package! irony-eldoc :after irony)

(def-package! flycheck-irony
  :when (featurep! :feature syntax-checker)
  :after irony
  :config (flycheck-irony-setup))


;;
;; Tools
;;

(def-package! disaster :commands disaster)


;;
;; Major modes
;;

(def-package! cmake-mode
  :mode "CMakeLists\\.txt$"
  :config
  (set! :company-backend 'cmake-mode '(company-cmake company-yasnippet)))

(def-package! cuda-mode :mode "\\.cuh?$")

(def-package! opencl-mode :mode "\\.cl$")

(def-package! demangle-mode
  :commands demangle-mode
  :init (add-hook 'llvm-mode-hook #'demangle-mode))

(def-package! glsl-mode
  :mode "\\.glsl$"
  :mode "\\.vert$"
  :mode "\\.frag$"
  :mode "\\.geom$")


;;
;; Plugins
;;

(when (featurep! :completion company)
  (def-package! company-cmake :after cmake-mode)

  (def-package! company-irony :after irony)

  (def-package! company-irony-c-headers :after company-irony)

  (def-package! company-glsl
    :when (featurep! :completion company)
    :after glsl-mode
    :config
    (if (executable-find "glslangValidator")
        (warn "glsl-mode: couldn't find glslangValidator, disabling company-glsl")
      (set! :company-backend 'glsl-mode '(company-glsl)))))
