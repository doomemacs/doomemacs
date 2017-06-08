;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +cc/autoclose->-maybe ()
  "For some reason smartparens won't autoskip >'s, this hack does."
  (interactive)
  (if (save-excursion
        (backward-char)
        (looking-at-p "[^ \t]>"))
      (forward-char)
    (call-interactively 'self-insert-command)))

(defun +cc--copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

;;;###autoload
(defun +cc|extra-fontify-c++ ()
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
(defun +cc|extra-fontify-c/c++ ()
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
(defun +cc-sp-point-is-template-p (id action context)
  (and (sp-in-code-p id action context)
       (sp-point-after-word-p id action context)))

;;;###autoload
(defun +cc-sp-point-after-include-p (id action context)
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[ 	]*#include[^<]+"))))
