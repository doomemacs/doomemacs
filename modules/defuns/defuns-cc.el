;;; defuns-cc.el --- for module-cc.el

(defun doom--c-lineup-inclass (langelem)
  (let ((inclass (assoc 'inclass c-syntactic-context)))
    (save-excursion
      (goto-char (c-langelem-pos inclass))
      (if (or (looking-at "struct")
              (looking-at "typedef struct"))
          '+
        '++))))

;;;###autoload
(defun doom/c-lineup-arglist (orig-fun &rest args)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (if (and (eq major-mode 'c++-mode)
           (ignore-errors
             (save-excursion
               (goto-char (c-langelem-pos langelem))
               ;; Detect "[...](" or "[...]{". preceded by "," or "(",
               ;;   and with unclosed brace.
               (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
      0                           ; no additional indent
    (apply orig-fun args)))

;;;###autoload
(defun doom|init-c/c++-settings ()
  (when (memq major-mode '(c-mode c++-mode objc-mode))
    (c-toggle-electric-state -1)
    (c-toggle-auto-newline -1)
    (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
    (c-set-offset 'inline-open '+)
    (c-set-offset 'block-open '+)
    (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
    (c-set-offset 'case-label '+)        ; indent case labels by c-indent-level, too
    (c-set-offset 'access-label '-)
    (c-set-offset 'inclass 'doom--c-lineup-inclass)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close '0)
    ;; Certain mappings interfere with smartparens and custom bindings
    (define-key c-mode-map (kbd "DEL") nil)
    (define-key c-mode-base-map "#" 'self-insert-command)
    (define-key c-mode-base-map "{" 'self-insert-command)
    (define-key c-mode-base-map "}" 'self-insert-command)
    (define-key c-mode-base-map "/" 'self-insert-command)
    (define-key c-mode-base-map "*" 'self-insert-command)
    (define-key c-mode-base-map ";" 'self-insert-command)
    (define-key c-mode-base-map "," 'self-insert-command)
    (define-key c-mode-base-map ":" 'self-insert-command)
    (define-key c-mode-base-map "(" 'self-insert-command)
    (define-key c-mode-base-map ")" 'self-insert-command)

    (define-key c++-mode-map "}" nil)
    ;; FIXME: fix smartparens
    ;; (define-key c++-mode-map ">" nil)
    (map! :map (c-mode-base-map c++-mode-map) :i ">" 'doom/autoclose->-maybe)
    (define-key c++-mode-map "<" nil)))

;;;###autoload
(defun doom/autoclose->-maybe ()
  "For some reason smartparens won't autoskip >'s, this hack does."
  (interactive)
  (if (save-excursion
        (backward-char)
        (looking-at-p "[^ \t]>"))
      (forward-char)
    (call-interactively 'self-insert-command)))

(defun doom--copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

;;;###autoload
(defun doom|init-c++-C11-highlights ()
  ;; C++11 syntax support (until cc-mode is updated)
  (require 'font-lock)
  ;; labels, case, public, private, protected, namespace-tags
  (doom--copy-face 'font-lock-label-face 'font-lock-keyword-face)
  ;; comment markups such as Javadoc-tags
  (doom--copy-face 'font-lock-doc-markup-face 'font-lock-doc-face)
  ;; comment markups
  (doom--copy-face 'font-lock-doc-string-face 'font-lock-comment-face)
  (setq font-lock-maximum-decoration t)

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
         ) t))

;;;###autoload
(defun doom/append-semicolon ()
  "Append a semicolon to the end of this (or each selected) non-empty line."
  (interactive)
  (let ((beg (if (evil-visual-state-p) evil-visual-beginning (line-beginning-position)))
        (end (if (evil-visual-state-p) evil-visual-end (line-end-position))))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((lend (save-excursion (evil-last-non-blank) (point))))
          (goto-char (1+ lend))
          (unless (or (eq (char-before) ?\;)
                      (= lend (line-beginning-position)))
            (insert ";")))
        (forward-line)))
    (when (evil-visual-state-p)
      (evil-normal-state))))

;;;###autoload
(defun doom/sp-point-is-template-p (id action context)
  (and (sp-in-code-p id action context)
       (sp-point-after-word-p id action context)))

;;;###autoload
(defun doom/sp-point-after-include-p (id action context)
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[ 	]*#include[^<]+"))))

(provide 'defuns-cc)
;;; defuns-cc.el ends here
