;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))


;;
;; Library

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
  (and (eq major-mode 'c++-mode)
       (or (assoc 'access-label c-syntactic-context)
           (save-excursion
             (save-match-data
               (re-search-backward
                "\\(?:p\\(?:ublic\\|r\\(?:otected\\|ivate\\)\\)\\)"
                (c-langelem-pos langelem) t))))
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

(defun +cc--re-search-for (regexp)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (re-search-forward regexp magic-mode-regexp-match-limit t)))))

;;;###autoload
(defun +cc-c-c++-objc-mode ()
  "Sets either `c-mode', `objc-mode' or `c++-mode', whichever is appropriate."
  (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer)))))
    (cond ((file-exists-p! (or (concat base ".cpp")
                               (concat base ".cc")))
           (c++-mode))
          ((or (file-exists-p! (or (concat base ".m")
                                   (concat base ".mm")))
               (+cc--re-search-for
                (concat "^[ \t\r]*\\(?:"
                        "@\\(?:class\\|interface\\|property\\|end\\)\\_>"
                        "\\|#import +<Foundation/Foundation.h>"
                        "\\|[-+] ([a-zA-Z0-9_]+)"
                        "\\)")))
           (objc-mode))
          ((fboundp 'c-or-c++-mode) ; introduced in Emacs 26.1
           (c-or-c++-mode))
          ((+cc--re-search-for  ; TODO Remove this along with Emacs 25 support
            (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
              (concat "^" ws-maybe "\\(?:"
                      "using"     ws "\\(?:namespace" ws "std;\\|std::\\)"
                      "\\|" "namespace" "\\(:?" ws id "\\)?" ws-maybe "{"
                      "\\|" "class"     ws id ws-maybe "[:{\n]"
                      "\\|" "template"  ws-maybe "<.*>"
                      "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
                      "\\)")))
           (c++-mode))
          ((functionp +cc-default-header-file-mode)
           (funcall +cc-default-header-file-mode))
          ((c-mode)))))


;;
;; Commands

;;;###autoload
(defun +cc/reload-compile-db ()
  "Reload the current project's JSON compilation database."
  (interactive)
  (unless (memq major-mode '(c-mode c++-mode objc-mode))
    (user-error "Not a C/C++/ObjC buffer"))
  ;; first rtag
  (when (and (featurep 'rtags)
             rtags-enabled
             (executable-find "rc"))
    (with-temp-buffer
      (message "Reloaded compile commands for rtags daemon")
      (rtags-call-rc :silent t "-J" (or (doom-project-root) default-directory))))
  ;; then irony
  (when (and (featurep 'irony) irony-mode)
    (+cc|irony-init-compile-options)))

;;;###autoload
(defun +cc/imenu ()
  "Invoke `rtags-imenu' if a running rdm process is available, otherwise invoke
`imenu'."
  (interactive)
  (call-interactively
   (if (and (processp rtags-rdm-process)
            (not (eq (process-status rtags-rdm-process) 'exit))
            (not (eq (process-status rtags-rdm-process) 'signal)))
       #'rtags-imenu
     #'imenu)))


;;
;; Hooks

;;;###autoload
(defun +cc|fontify-constants ()
  "Better fontification for preprocessor constants"
  (when (memq major-mode '(c-mode c++-mode))
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
     t)))

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
               (list (locate-dominating-file (or buffer-file-name default-directory)
                                             "include"))
               (cl-loop for path in +cc-default-include-paths
                        nconc (list "-I" path)))
       (doom-project-root)))
    ;; Make ffap aware of include paths
    (when irony--working-directory
      (require 'ffap)
      (make-local-variable 'ffap-c-path)
      (make-local-variable 'ffap-c++-path)
      (cl-loop for opt in irony--compile-options
               if (string-match "^-I\\(.+\\)" opt)
               do (add-to-list (pcase major-mode
                                 (`c-mode 'ffap-c-path)
                                 (`c++-mode 'ffap-c++-path))
                               (expand-file-name (match-string 1 opt)
                                                 irony--working-directory))))))

;;;###autoload
(defun +cc|cleanup-rtags ()
  "Kill rtags server(s) if there are no C/C++ buffers open."
  (unless (doom-buffers-in-mode '(c-mode c++-mode) (buffer-list))
    (rtags-cancel-process)))
