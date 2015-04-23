(use-package cmake-mode
  :mode "CMakeLists\\.txt$"
  :config
  (progn
    (after "auto-complete" (add-to-list 'ac-modes 'cmake-mode))
    (after "company"
      (use-package company-cmake
        :config (company--backend-on 'cmake-mode-hook 'company-cmake 'company-yasnippet)))))

;; Shaders
(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))

(use-package cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :init
  (progn
    (associate-mode "\\.h$" 'c++-mode)
    (associate-mode "\\.mm$" 'objc-mode))
  :config
  (progn
    (setq c-basic-offset 4
          c-tab-always-indent nil)

    (when is-mac
      (setq my--clang-includes
            '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
              "/usr/local/include"
              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.0/include"
              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
              "/usr/include"
              "/System/Library/Frameworks"
              "/Library/Frameworks"))

      (defun my--clang-includes () my--clang-includes)
      (defun my--clang-includes-flags ()
        (mapcar (lambda (item) (concat "-I" item)) my--clang-includes)))

    (progn ; C++
      (defun my--setup-c++-mode-flycheck ()
        (setq flycheck-clang-language-standard "c++11"
              flycheck-clang-standard-library "libc++"
              flycheck-c/c++-clang-executable "clang++"
              ;; flycheck-clang-include-path (my--clang-includes)
              ))
      (after "flycheck"
        (add-hook 'c++-mode-hook 'my--setup-c++-mode-flycheck)))

    (progn ; Obj-C
      (add-to-list 'magic-mode-alist
                   `(,(lambda ()
                        (and (string= (file-name-extension buffer-file-name) "h")
                             (re-search-forward "@\\<interface\\>"
                                                magic-mode-regexp-match-limit t)))
                     . objc-mode))
      (after "flycheck" (add-hook! 'objc-mode-hook (use-package flycheck-objc))))

    (after "company"
      ;; TODO Clang is *really* slow in larger projects, maybe replace it with irony-mode or ycmd?
      (company--backend-on 'c-mode-hook 'company-c-headers 'company-clang)
      (company--backend-on 'c++-mode-hook 'company-c-headers 'company-clang)
      (company--backend-on 'objc-mode-hook 'company-c-headers 'company-xcode))

    (add-hook! 'c-mode-common-hook
      (c-toggle-electric-state -1)
      (c-toggle-auto-newline -1)
      (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
      (c-set-offset 'inline-open '+)
      (c-set-offset 'block-open '+)
      (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
      (c-set-offset 'case-label '+)        ; indent case labels by c-indent-level, too
      (c-set-offset 'access-label '-)
      (c-set-offset 'inclass '++)

      ;; DEL mapping interferes with smartparens and my.deflate-maybe
      (bind c-mode-map (kbd "DEL") nil))

    ;; C++11 syntax support (until cc-mode is updated)
    (require 'font-lock)
    (defun --copy-face (new-face face)
      "Define NEW-FACE from existing FACE."
      (copy-face face new-face)
      (eval `(defvar ,new-face nil))
      (set new-face new-face))

    (--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
                 'font-lock-keyword-face)
    (--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
                 'font-lock-doc-face)
    (--copy-face 'font-lock-doc-string-face ; comment markups
                 'font-lock-comment-face)
    (global-font-lock-mode t)
    (setq font-lock-maximum-decoration t)

    (add-hook! 'c++-mode-hook
      ;; We could place some regexes into `c-mode-common-hook', but
      ;; note that their evaluation order matters.
      (font-lock-add-keywords
       nil '(;; complete some fundamental keywords
             ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
             ;; namespace names and tags - these are rendered as constants by cc-mode
             ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
             ;;  new C++11 keywords
             ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
             ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
             ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
             ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
             ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
             ;; hexadecimal numbers
             ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
             ;; integer/float/scientific numbers
             ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
             ;; c++11 string literals
             ;;       L"wide string"
             ;;       L"wide string with UNICODE codepoint: \u2018"
             ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
             ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
             ;;       R"(user-defined literal)"
             ;;       R"( a "quot'd" string )"
             ;;       R"delimiter(The String Data" )delimiter"
             ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
             ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
             (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
             (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

             ;; user-defined types (rather project-specific)
             ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
             ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
             )) t)

    ;; Fix enum and C++11 lambda indentation
    (defadvice c-lineup-arglist (around c-lineup-arglist-indent-fix activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                           ; no additional indent
              ad-do-it)))                   ; default behavior
    ))


(provide 'init-cc)
;;; init-cc.el ends here
