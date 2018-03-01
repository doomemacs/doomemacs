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
(defun +cc-sp-point-is-template-p (id action context)
  "Return t if point is in the right place for C++ angle-brackets."
  (and (sp-in-code-p id action context)
       (cond ((eq action 'insert)
              (sp-point-after-word-p id action context))
             ((eq action 'autoskip)
              (/= (char-before) 32)))))

;;;###autoload
(defun +cc-sp-point-after-include-p (id action context)
  "Return t if point is in an #include."
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[ 	]*#include[^<]+"))))

;;;###autoload
(defun +cc-c++-lineup-inclass (langelem)
  "Indent inclass lines one level further than access modifier keywords."
  (when (and (eq major-mode 'c++-mode)
             (or (assoc 'access-label c-syntactic-context)
                 (save-excursion
                   (save-match-data
                     (re-search-backward
                      "\\(?:p\\(?:ublic\\|r\\(?:otected\\|ivate\\)\\)\\)"
                      (c-langelem-pos langelem) t)))))
    '++))

;;;###autoload
(defun +cc-lineup-arglist-close (langlem)
  "Line up the closing brace in an arglist with the opening brace IF cursor is
preceded by the opening brace or a comma (disregarding whitespace in between)."
  (when (save-excursion
          (save-match-data
            (skip-chars-backward " \t\n" (c-langelem-pos langelem))
            (memq (char-before) (list ?, ?\( ?\;))))
    (c-lineup-arglist langlem)))


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

;;;###autoload
(defun +cc|init-rtags ()
  "Start an rtags server in c-mode and c++-mode buffers."
  (when (memq major-mode '(c-mode c++-mode))
    (rtags-start-process-unless-running)))

;;;###autoload
(defun +cc|cleanup-rtags ()
  "Kill rtags server(s) if there are no C/C++ buffers open."
  (unless (doom-buffers-in-mode '(c-mode c++-mode) (buffer-list))
    (rtags-cancel-process)))
