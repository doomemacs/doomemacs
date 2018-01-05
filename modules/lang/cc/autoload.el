;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +cc/reload-compile-db ()
  "Reload the current project's JSON compilation database."
  (interactive)
  (unless (memq major-mode '(c-mode c++-mode objc-mode))
    (user-error "Not a C/C++/ObjC buffer"))
  (unless (doom-project-has! "compile_commands.json")
    (user-error "No compile_commands.json file"))
  ;; first rtag
  (when (and (featurep 'rtags)
             rtags-enabled
             (executable-find "rc"))
    (with-temp-buffer
      (message "Reloaded compile commands for rtags daemon")
      (rtags-call-rc :silent t "-J" (doom-project-root))))
  ;; then irony
  (when (and (featurep 'irony) irony-mode)
    (+cc|irony-init-compile-options)))

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
(defun +cc|irony-init-compile-options ()
  "Initialize compiler options for irony-mode. It searches for the nearest
compilation database and initailizes it, otherwise falling back on
`+cc-default-compiler-options' and `+cc-default-include-paths'.

See https://github.com/Sarcasm/irony-mode#compilation-database for details on
compilation dbs."
  (when (memq major-mode '(c-mode c++-mode objc-mode))
    (require 'irony-cdb)
    (unless (irony-cdb-autosetup-compile-options)
      (irony-cdb--update-compile-options
       (append (delq nil (cdr-safe (assq major-mode +cc-default-compiler-options)))
               (cl-loop for path in +cc-default-include-paths
                        nconc (list "-I" path)))
       (doom-project-root)))))

