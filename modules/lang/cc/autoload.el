;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))

;; The plusses in c++-mode can be annoying to search for ivy/helm (which reads
;; queries as regexps), so we add these for convenience.
;;;###autoload (defalias 'cpp-mode 'c++-mode)
;;;###autoload (defvaralias 'cpp-mode-map 'c++-mode-map)


;;
;; Library

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
  "Uses heuristics to detect `c-mode', `objc-mode' or `c++-mode'.

1. Checks if there are nearby cpp/cc/m/mm files with the same name.
2. Checks for ObjC and C++-specific keywords and libraries.
3. Falls back to `+cc-default-header-file-mode', if set.
4. Otherwise, activates `c-mode'.

This is meant to replace `c-or-c++-mode' (introduced in Emacs 26.1), which
doesn't support specification of the fallback mode and whose heuristics are
simpler."
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
          ((+cc--re-search-for
            (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
              (concat "^" ws-maybe "\\(?:"
                      "using" ws "\\(?:namespace" ws "std;\\|std::\\)"
                      "\\|" "namespace" "\\(?:" ws id "\\)?" ws-maybe "{"
                      "\\|" "class"     ws id ws-maybe "[:{\n]"
                      "\\|" "template"  ws-maybe "<.*>"
                      "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
                      "\\)")))
           (c++-mode))
          ((functionp +cc-default-header-file-mode)
           (funcall +cc-default-header-file-mode))
          ((c-mode)))))

(defun +cc-resolve-include-paths ()
  (cl-loop with path = (or buffer-file-name default-directory)
           for dir in +cc-default-include-paths
           if (file-name-absolute-p dir)
           collect dir
           else if (projectile-locate-dominating-file path dir)
           collect (expand-file-name dir it)))


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
             (executable-find rtags-rc-binary-name))
    (with-temp-buffer
      (message "Reloaded compile commands for rtags daemon")
      (rtags-call-rc :silent t "-J" (or (doom-project-root) default-directory))))
  ;; then irony
  (when (and (featurep 'irony) irony-mode)
    (+cc-init-irony-compile-options-h))
  ;; Otherwise, LSP
  (when (bound-and-true-p lsp-mode)
    (lsp-workspace-restart))
  (when (bound-and-true-p eglot-managed-mode)
    (eglot-reconnect)))

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

;; Eglot specific helper, courtesy of MaskRay
;;;###autoload
(defun +cc/eglot-ccls-show-inheritance-hierarchy (&optional derived)
  "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
  (interactive "P")
  (if-let* ((res (jsonrpc-request
                  (eglot--current-server-or-lose)
                  :$ccls/inheritance
                  (append (eglot--TextDocumentPositionParams)
                          `(:derived ,(if derived t :json-false))
                          '(:levels 100) '(:hierarchy t))))
            (tree (list (cons 0 res))))
      (with-help-window "*ccls inheritance*"
        (with-current-buffer standard-output
          (while tree
            (pcase-let ((`(,depth . ,node) (pop tree)))
              (cl-destructuring-bind (&key uri range) (plist-get node :location)
                (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                (make-text-button (+ (point-at-bol 0) depth) (point-at-eol 0)
                                  'action (lambda (_arg)
                                            (interactive)
                                            (find-file (eglot--uri-to-path uri))
                                            (goto-char (car (eglot--range-region range)))))
                (cl-loop for child across (plist-get node :children)
                         do (push (cons (1+ depth) child) tree)))))))
    (eglot--error "Hierarchy unavailable")))

;;;###autoload
(defun +cc-cmake-lookup-documentation-fn (_)
  "Look up the symbol at point in CMake's documentation."
  (condition-case _
      (progn
        (save-window-excursion (cmake-help))
        (when-let (buf (get-buffer "*CMake Help*"))
          (pop-to-buffer buf)
          t))
    (error nil)))


;;
;; Hooks

;;;###autoload
(defun +cc-fontify-constants-h ()
  "Better fontification for preprocessor constants"
  (when (memq major-mode '(c-mode c++-mode))
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[0-9A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
     t)))

(defvar +cc--project-includes-alist nil)
;;;###autoload
(defun +cc-init-irony-compile-options-h ()
  "Initialize compiler options for irony-mode. It searches for the nearest
compilation database and initailizes it, otherwise falling back on
`+cc-default-compiler-options' and `+cc-default-include-paths'.

See https://github.com/Sarcasm/irony-mode#compilation-database for details on
compilation dbs."
  (when (memq major-mode '(c-mode c++-mode objc-mode))
    (require 'irony-cdb)
    (unless (irony-cdb-autosetup-compile-options)
      (let ((project-root (doom-project-root))
            (include-paths (+cc-resolve-include-paths)))
        (setf (alist-get project-root +cc--project-includes-alist)
              include-paths)
        (irony-cdb--update-compile-options
         (append (delq nil (cdr-safe (assq major-mode +cc-default-compiler-options)))
                 (cl-loop for path in include-paths
                          collect (format "-I%s" path)))
         project-root)))))

;; ;;;###autoload
;; (defun +cc|init-ccls-compile-options ()
;;   "TODO"
;;   (when (memq major-mode '(c-mode c++-mode objc-mode))
;;     (when-let (include-paths (+cc-resolve-include-paths))
;;       (let ((args (delq nil (cdr-safe (assq major-mode +cc-default-compiler-options)))))
;;         (setf (alist-get (or (lsp-workspace-root)
;;                              (lsp--suggest-project-root)
;;                              (doom-project-root))
;;                          +cc--project-includes-alist)
;;               include-paths)
;;         (setq ccls-initialization-options
;;               `(:clang (:extraArgs
;;                         [,@(cl-loop for path in include-paths
;;                                     collect (format "-I%s" path))])))))))

;;;###autoload
(defun +cc-init-ffap-integration-h ()
  "Takes the local project include paths and registers them with ffap.
This way, `find-file-at-point' (and `+lookup/file') will know where to find most
header files."
  (when-let (project-root (or (bound-and-true-p irony--working-directory)
                              (and (featurep 'lsp)
                                   (or (lsp-workspace-root)
                                       (doom-project-root)))))
    (require 'ffap)
    (make-local-variable 'ffap-c-path)
    (make-local-variable 'ffap-c++-path)
    (cl-loop for dir in (or (cdr (assoc project-root +cc--project-includes-alist))
                            (+cc-resolve-include-paths))
             do (add-to-list (pcase major-mode
                               (`c-mode 'ffap-c-path)
                               (`c++-mode 'ffap-c++-path))
                             (expand-file-name dir project-root)))))


;;
;;; CCLS specific helpers

;; ccls-show-vars ccls-show-base ccls-show-derived ccls-show-members have a
;; parameter while others are interactive.
;;
;; (+cc/ccls-show-base 1) direct bases
;; (+cc/ccls-show-derived 1) direct derived
;; (+cc/ccls-show-member 2) => 2 (Type) => nested classes / types in a namespace
;; (+cc/ccls-show-member 3) => 3 (Func) => member functions / functions in a namespace
;; (+cc/ccls-show-member 0) => member variables / variables in a namespace
;; (+cc/ccls-show-vars 1) => field
;; (+cc/ccls-show-vars 2) => local variable
;; (+cc/ccls-show-vars 3) => field or local variable. 3 = 1 | 2
;; (+cc/ccls-show-vars 4) => parameter

;;;###autoload
(defun +cc/ccls-show-callee ()
  "Show callees of symbol under point."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))

;;;###autoload
(defun +cc/ccls-show-caller ()
  "Show callers of symbol under point."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))

;;;###autoload
(defun +cc/ccls-show-vars (kind)
  "Show variables of type KIND as symbol under point.
   1 -> field
   2 -> local variable
   3 -> field or local variables. 3 = 1 | 2.
   4 -> parameter"
  (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))

;;;###autoload
(defun +cc/ccls-show-base (levels)
  "Show bases of class under point up to LEVELS levels (1 for direct bases)."
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))

;;;###autoload
(defun +cc/ccls-show-derived (levels)
  "Show derived classes from class under point down to LEVELS levels (1 for direct derived)."
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))

;;;###autoload
(defun +cc/ccls-show-member (kind)
  "Show member elements of kind KIND for class/namespace under point.
   0 -> member variables/ variables in a namespace
   2 -> nested classes / types in a namespace
   3 -> member functions / functions in a namespace"
  (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h
;;;###autoload
(defun +cc/ccls-show-references-address ()
  "References w/ Role::Address bit (e.g. variables explicitly being taken addresses)"
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 128)))

;;;###autoload
(defun +cc/ccls-show-references-macro ()
  "References w/ Role::Dynamic bit (macro expansions)"
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 64)))

;;;###autoload
(defun +cc/ccls-show-references-not-call ()
  "References w/o Role::Call bit (e.g. where functions are taken addresses)"
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :excludeRole 32)))

;;;###autoload
(defun +cc/ccls-show-references-read ()
  "References w/ Role::Read"
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 8)))

;;;###autoload
(defun +cc/ccls-show-references-write ()
  "References w/ Role::Write"
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :role 16)))
