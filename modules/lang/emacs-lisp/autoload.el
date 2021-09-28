;;; lang/emacs-lisp/autoload.el -*- lexical-binding: t; -*-

;;
;;; Library

;;;###autoload
(defun +emacs-lisp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer."
  (+eval-display-results
   (string-trim-right
    (condition-case-unless-debug e
        (let ((result
               (let ((buffer-file-name
                      (buffer-file-name (buffer-base-buffer)))
                     (doom--current-module
                      (ignore-errors (doom-module-from-path buffer-file-name)))
                     (debug-on-error t))
                 (eval (read (format "(progn %s)"
                                     (buffer-substring-no-properties beg end)))
                       lexical-binding))))
          (require 'pp)
          (replace-regexp-in-string "\\\\n" "\n" (pp-to-string result)))
      (error (error-message-string e))))
   (current-buffer)))


;;
;;; Handlers

(defun +emacs-lisp--module-at-point ()
  "Return (CATEGORY MODULE FLAG) at point inside a `doom!' block."
  (let ((origin (point))
        (syntax (syntax-ppss)))
    (when (and (> (ppss-depth syntax) 0) (not (ppss-string-terminator syntax)))
      (save-excursion
        (let ((parens (ppss-open-parens syntax))
              (doom-depth 1))
          (while (and parens (progn (goto-char (car parens))
                                    (not (looking-at "(doom!\\_>"))))
            (setq parens (cdr parens)
                  doom-depth (1+ doom-depth)))
          (when parens ;; Are we inside a `doom!' block?
            (goto-char origin)
            (let* ((doom-start (car parens))
                   (bare-symbol
                    (if (ppss-comment-depth syntax)
                        (= (save-excursion (beginning-of-thing 'list)) doom-start)
                      (null (cdr parens))))
                   (sexp-start (if bare-symbol
                                   (beginning-of-thing 'symbol)
                                 (or (cadr parens) (beginning-of-thing 'list))))
                   (match-start nil))
              (goto-char sexp-start)
              (while (and (not match-start)
                          (re-search-backward
                           "\\_<:\\(?:\\sw\\|\\s_\\)+\\_>" ;; Find a keyword.
                           doom-start 'noerror))
                (unless (looking-back "(")
                  (let ((kw-syntax (syntax-ppss)))
                    (when (and (= (ppss-depth kw-syntax) doom-depth)
                               (not (ppss-string-terminator kw-syntax))
                               (not (ppss-comment-depth kw-syntax)))
                      (setq match-start (point))))))
              (when match-start
                (let (category module flag)
                  ;; `point' is already at `match-start'.
                  (setq category (symbol-at-point))
                  (goto-char origin)
                  (if bare-symbol
                      (setq module (symbol-at-point))
                    (let ((symbol (symbol-at-point))
                          (head (car (list-at-point))))
                      (if (and (symbolp head) (not (keywordp head))
                               (not (eq head symbol)))
                          (setq module head
                                flag symbol)
                        (setq module symbol))))
                  (list category module flag))))))))))

;;;###autoload
(defun +emacs-lisp-lookup-definition (_thing)
  "Lookup definition of THING."
  (if-let (module (+emacs-lisp--module-at-point))
      (doom/help-modules (car module) (cadr module) 'visit-dir)
    (call-interactively #'elisp-def)))

;;;###autoload
(defun +emacs-lisp-lookup-documentation (thing)
  "Lookup THING with `helpful-variable' if it's a variable, `helpful-callable'
if it's callable, `apropos' otherwise."
  (cond ((when-let (module (+emacs-lisp--module-at-point))
           (doom/help-modules (car module) (cadr module))
           (when (eq major-mode 'org-mode)
             (with-demoted-errors "%s"
               (re-search-forward
                (if (caddr module)
                    "\\* Module Flags$"
                  "\\* Description$"))
               (when (caddr module)
                 (re-search-forward (format "=\\%s=" (caddr module))
                                    nil t))
               (when (invisible-p (point))
                 (org-show-hidden-entry))))
           'deferred))
        (thing (helpful-symbol (intern thing)))
        ((call-interactively #'helpful-at-point))))

;;;###autoload
(defun +emacs-lisp-indent-function (indent-point state)
  "A replacement for `lisp-indent-function'.

Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
  (let ((normal-indent (current-column))
        (orig-point (point))
        ;; TODO Refactor `target' usage (ew!)
        target)
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond ((and (elt state 2)
                (or (not (looking-at-p "\\sw\\|\\s_"))
                    (eq (char-after) ?:)))
           (unless (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp)
             (goto-char calculate-lisp-indent-last-sexp)
             (beginning-of-line)
             (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
           (backward-prefix-chars)
           (current-column))
          ((and (save-excursion
                  (goto-char indent-point)
                  (skip-syntax-forward " ")
                  (not (eq (char-after) ?:)))
                (save-excursion
                  (goto-char orig-point)
                  (and (eq (char-after) ?:)
                       (eq (char-before) ?\()
                       (setq target (current-column)))))
           (save-excursion
             (move-to-column target t)
             target))
          ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                  (method (or (function-get (intern-soft function) 'lisp-indent-function)
                              (get (intern-soft function) 'lisp-indent-hook))))
             (cond ((or (eq method 'defun)
                        (and (null method)
                             (> (length function) 3)
                             (string-match-p "\\`def" function)))
                    (lisp-indent-defform state indent-point))
                   ((integerp method)
                    (lisp-indent-specform method state indent-point normal-indent))
                   (method
                    (funcall method indent-point state))))))))


;;
;;; Commands

;;;###autoload
(defun +emacs-lisp/open-repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*ielm*")
       (progn (ielm)
              (let ((buf (get-buffer "*ielm*")))
                (bury-buffer buf)
                buf)))))

;;;###autoload
(defun +emacs-lisp/buttercup-run-file ()
  "Run all buttercup tests in the focused buffer."
  (interactive)
  (let ((load-path
         (append (list (doom-path (dir!) "..")
                       (or (doom-project-root)
                           default-directory))
                 load-path))
        (buttercup-suites nil))
    (save-selected-window
      (eval-buffer)
      (buttercup-run))
    (message "File executed successfully")))

;;;###autoload
(defun +emacs-lisp/buttercup-run-project ()
  "Run all buttercup tests in the project."
  (interactive)
  (let* ((default-directory (doom-project-root))
         (load-path (append (list (doom-path "test")
                                  default-directory)
                            load-path))
         (buttercup-suites nil))
    (buttercup-run-discover)))

;;;###autoload
(defun +emacs-lisp/edebug-instrument-defun-on ()
  "Toggle on instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun 'edebugit))

;;;###autoload
(defun +emacs-lisp/edebug-instrument-defun-off ()
  "Toggle off instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun nil))


;;
;;; Hooks

(autoload 'straight-register-file-modification "straight")
;;;###autoload
(defun +emacs-lisp-init-straight-maybe-h ()
  "Make sure straight sees modifications to installed packages."
  (when (file-in-directory-p (or buffer-file-name default-directory) doom-local-dir)
    (add-hook 'after-save-hook #'straight-register-file-modification
              nil 'local)))

;;;###autoload
(defun +emacs-lisp-extend-imenu-h ()
  "Improve imenu support in `emacs-lisp-mode', including recognition for Doom's API."
  (setq imenu-generic-expression
        `(("Section" "^[ \t]*;;;;*[ \t]+\\([^\n]+\\)" 1)
          ("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
          ("Package" "^\\s-*(\\(?:;;;###package\\|package!\\|use-package!?\\|after!\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
          ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
          ("Modelines" "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
          ("Modeline segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
          ("Advice" "^\\s-*(\\(?:def\\(?:\\(?:ine-\\)?advice!?\\)\\) +\\([^ )\n]+\\)" 1)
          ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
          ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
          ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
          ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
          ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))))

;;;###autoload
(defun +emacs-lisp-reduce-flycheck-errors-in-emacs-config-h ()
  "Remove `emacs-lisp-checkdoc' checker and reduce `emacs-lisp' checker
verbosity when editing a file in `doom-private-dir' or `doom-emacs-dir'."
  (when (and (bound-and-true-p flycheck-mode)
             (eq major-mode 'emacs-lisp-mode)
             (or (not default-directory)
                 (null (buffer-file-name (buffer-base-buffer)))
                 (cl-find-if (doom-partial #'file-in-directory-p default-directory)
                             +emacs-lisp-disable-flycheck-in-dirs)))
    (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
    (set (make-local-variable 'flycheck-emacs-lisp-check-form)
         (concat "(progn "
                 (prin1-to-string
                  `(progn
                     (setq doom-modules ',doom-modules
                           doom-disabled-packages ',doom-disabled-packages)
                     (ignore-errors (load ,user-init-file t t))
                     (setq byte-compile-warnings
                           '(obsolete cl-functions
                             interactive-only make-local mapcar
                             suspicious constants))
                     (defmacro map! (&rest _))))
                 " "
                 (default-value 'flycheck-emacs-lisp-check-form)
                 ")"))))


;;
;;; Fontification

;;;###autoload
(defun +emacs-lisp-truncate-pin ()
  "Truncates long SHA1 hashes in `package!' :pin's."
  (save-excursion
    (goto-char (match-beginning 0))
    (and (stringp (plist-get (sexp-at-point) :pin))
         (search-forward ":pin" nil t)
         (let ((start (re-search-forward "\"[^\"\n]\\{12\\}" nil t))
               (finish (and (re-search-forward "\"" (line-end-position) t)
                            (match-beginning 0))))
           (when (and start finish)
             (put-text-property start finish 'display "...")))))
  nil)

(defvar +emacs-lisp--face nil)
;;;###autoload
(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq +emacs-lisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq +emacs-lisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

;; HACK Fontification is already expensive enough. We byte-compile
;;      `+emacs-lisp-highlight-vars-and-faces' and `+emacs-lisp-truncate-pin' to
;;      ensure they run as fast as possible:
(dolist (fn '(+emacs-lisp-highlight-vars-and-faces +emacs-lisp-truncate-pin))
  (unless (byte-code-function-p (symbol-function fn))
    (with-no-warnings (byte-compile fn))))
