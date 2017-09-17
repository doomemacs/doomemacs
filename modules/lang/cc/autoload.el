;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +cc*align-lambda-arglist (orig-fun &rest args)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (if (and (eq major-mode 'c++-mode)
           (ignore-errors
             (save-excursion
               (goto-char (c-langelem-pos langelem))
               ;; Detect "[...](" or "[...]{". preceded by "," or "(",
               ;;   and with unclosed brace.
               (looking-at-p ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
      0 ; no additional indent
    (apply orig-fun args)))

;;;###autoload
(defun +cc/autoclose->-maybe ()
  "For some reason smartparens won't autoskip >'s, this hack does."
  (interactive)
  (if (save-excursion
        (backward-char)
        (looking-at-p "[^ \t]>"))
      (forward-char)
    (call-interactively #'self-insert-command)))

;;;###autoload
(defun +cc-sp-point-is-template-p (id action context)
  "Return t if point is in the right place for C++ angle-brackets."
  (and (sp-in-code-p id action context)
       (sp-point-after-word-p id action context)))

;;;###autoload
(defun +cc-sp-point-after-include-p (id action context)
  "Return t if point is in an #include."
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[ 	]*#include[^<]+"))))

;;;###autoload
(defun +cc-c-lineup-inclass (_langelem)
  "Indent privacy keywords at same level as class properties."
  (if (memq major-mode '(c-mode c++-mode))
      (let ((inclass (assq 'inclass c-syntactic-context)))
        (save-excursion
          (goto-char (c-langelem-pos inclass))
          (if (or (looking-at "struct")
                  (looking-at "typedef struct"))
              '+
            '++)))
    '+))


;;
;; Hooks
;;

;;;###autoload
(defun +cc|fontify-constants ()
  "Better fontification for preprocessor constants"
  (font-lock-add-keywords
   nil '(("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
         ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
   t))

;;;###autoload
(defun +cc|irony-add-include-paths ()
  "Seek out and add the nearest include/ folders to clang's options."
  (when-let (dir (locate-dominating-file buffer-file-name "include/"))
    (cl-pushnew (concat "-I" (expand-file-name "include/" dir))
                irony-additional-clang-options :test #'equal)))

;;;###autoload
(defun +cc|use-c++11 ()
  "Enable C++11 support with clang (via irony)."
  (cl-pushnew "-std=c++11" irony-additional-clang-options :test #'equal)
  (when IS-MAC
    ;; NOTE beware: you'll get abi-inconsistencies when passing std-objects to
    ;; libraries linked with libstdc++ (e.g. if you use boost which wasn't
    ;; compiled with libc++)
    (cl-pushnew "-stdlib=libc++" irony-additional-clang-options :test #'equal)))
