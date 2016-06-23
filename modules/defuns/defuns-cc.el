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
(defun doom|extra-fontify-c++ ()
  ;; We could place some regexes into `c-mode-common-hook', but
  ;; note that their evaluation order matters.
  ;; NOTE modern-cpp-font-lock will eventually supercede some of these rules
  (font-lock-add-keywords
   nil '(;; c++11 string literals
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
         ) t))

;;;###autoload
(defun doom|extra-fontify-c/c++ ()
  (font-lock-add-keywords
   nil '(;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
         ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
         ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
         ;; integer/float/scientific numbers
         ("\\<\\([\\-+]*[0-9\\.]+\\)\\>" 1 font-lock-constant-face t)
         ("\\<\\([\\-+]*[0-9\\.]+\\)\\(f\\)\\>"
          (1 font-lock-constant-face t)
          (2 font-lock-keyword-face t))
         ("\\<\\([\\-+]*[0-9\\.]+\\)\\([eE]\\)\\([\\-+]?[0-9]+\\)\\>"
          (1 font-lock-constant-face t)
          (2 font-lock-keyword-face t)
          (3 font-lock-constant-face t))
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
