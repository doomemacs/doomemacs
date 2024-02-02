;;; lang/emacs-lisp/autoload.el -*- lexical-binding: t; -*-

;;
;;; Library

;;;###autoload
(defun +emacs-lisp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer."
  (+eval-display-results
   (string-trim-right
    (let ((buffer (generate-new-buffer " *+eval-output*"))
          (debug-on-error t))
      (unwind-protect
          (condition-case-unless-debug e
              (doom-module-context-with
                  (doom-module-from-path
                   (or (buffer-file-name (buffer-base-buffer))
                       default-directory))
                (doom-context-with 'eval
                  (eval-region beg end buffer load-read-function))
                (with-current-buffer buffer
                  (let ((pp-max-width nil))
                    (require 'pp)
                    (pp-buffer)
                    (replace-regexp-in-string "\\\\n" "\n" (string-trim-left (buffer-string))))))
            (error (format "ERROR: %s" e)))
        (kill-buffer buffer))))
   (current-buffer)))

;;;###autoload
(defun +emacs-lisp-outline-level ()
  "Return outline level for comment at point.
Intended to replace `lisp-outline-level'."
  (- (match-end 1) (match-beginning 1)))


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
             (goto-char (point-min))
             (with-demoted-errors "%s"
               (re-search-forward
                (if (caddr module)
                    "^\\*+ Module flags"
                  "^\\* Description"))
               (when (caddr module)
                 (re-search-forward (format "=\\%s=" (caddr module))
                                    nil t))
               (when (memq (get-char-property (line-end-position)
                                              'invisible)
                           '(outline org-fold-outline))
                 (org-show-hidden-entry))))
           'deferred))
        (thing (helpful-symbol (intern thing)))
        ((call-interactively #'helpful-at-point))))

;; DEPRECATED Remove when 28 support is dropped.
(unless (fboundp 'lisp--local-defform-body-p)
  (fset 'lisp--local-defform-body-p #'ignore))

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
                (or (eq (char-after) ?:)
                    (not (looking-at-p "\\sw\\|\\s_"))))
           (if (lisp--local-defform-body-p state)
               (lisp-indent-defform state indent-point)
             (unless (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp)
               (goto-char calculate-lisp-indent-last-sexp)
               (beginning-of-line)
               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
             (backward-prefix-chars)
             (current-column)))
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
  "Improve imenu support in `emacs-lisp-mode' for Doom's APIs."
  (setq imenu-generic-expression
        `(("Section" "^[ \t]*;;;*\\**[ \t]+\\([^\n]+\\)" 1)
          ("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
          ("Package" "^\\s-*\\(?:;;;###package\\|(\\(?:package!\\|use-package!?\\|after!\\)\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
          ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
          ("Modelines" "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
          ("Modeline segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
          ("Advice" "^\\s-*(\\(?:def\\(?:\\(?:ine-\\)?advice!?\\)\\) +\\([^ )\n]+\\)" 1)
          ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
          ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
          ("CLI Command" "^\\s-*(\\(def\\(?:cli\\|alias\\|obsolete\\|autoload\\)! +\\([^\n]+\\)\\)" 1)
          ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
          ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
          ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))))

(defun +emacs-lisp--in-package-buffer-p ()
  (let* ((file-path (buffer-file-name (buffer-base-buffer)))
         (file-base (if file-path (file-name-base file-path))))
    (and (derived-mode-p 'emacs-lisp-mode)
         (or (null file-base)
             (locate-file file-base (custom-theme--load-path) '(".elc" ".el"))
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-max))
                 (when (re-search-backward "^ *\\((provide\\)\\(?:-theme\\)? +'"
                                           (max (point-min) (- (point-max) 512))
                                           t)
                   (goto-char (match-beginning 1))
                   (ignore-errors
                     (and (stringp file-base)
                          (equal (symbol-name (doom-unquote (nth 1 (read (current-buffer)))))
                                 file-base)))))))
         (not (locate-dominating-file default-directory ".doommodule")))))

(defvar-local +emacs-lisp-reduced-flymake-byte-compile--process nil)

(defun +emacs-lisp-reduced-flymake-byte-compile (report-fn &rest _args)
  "A Flymake backend for byte compilation in non-package elisp files.

This checker reduces the amount of false positives the byte compiler throws off
compared to `elisp-flymake-byte-compile'. The linter warnings that are enabled
are set by `+emacs-lisp-linter-warnings'

This backend does not need to be added directly
as `+emacs-lisp-non-package-mode' will enable it and disable the other checkers."
  ;; if a process already exists. kill it.
  (when (and +emacs-lisp-reduced-flymake-byte-compile--process
             (process-live-p +emacs-lisp-reduced-flymake-byte-compile--process))
    (kill-process +emacs-lisp-reduced-flymake-byte-compile--process))
  (let ((source (current-buffer))
        (tmp-file (make-temp-file "+emacs-lisp-byte-compile-src"))
        (out-buf (generate-new-buffer "+emacs-lisp-byte-compile-out")))
    ;; write the content to a temp file
    (save-restriction
      (widen)
      (write-region nil nil tmp-file nil 'nomessage))
    ;; make the process
    (setq +emacs-lisp-reduced-flymake-byte-compile--process
          (make-process
           :name "+emacs-reduced-flymake"
           :noquery t
           :connection-type 'pipe
           :buffer out-buf
           :command `(,(expand-file-name invocation-name invocation-directory)
                      "-Q"
                      "--batch"
                      ,@(mapcan (lambda (p) (list "-L" p)) elisp-flymake-byte-compile-load-path)
                      ;; this is what silences the byte compiler
                      "--eval" ,(prin1-to-string `(setq doom-modules ',doom-modules
                                                        doom-disabled-packages ',doom-disabled-packages
                                                        byte-compile-warnings ',+emacs-lisp-linter-warnings))
                      "-f" "elisp-flymake--batch-compile-for-flymake"
                      ,tmp-file)
           :stderr "*stderr of +elisp-flymake-byte-compile-out*"
           :sentinel
           ;; deal with the process when it exits
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   (cond
                    ;; if the buffer is dead or the process is not the same, log the process as old.
                    ((or (not (buffer-live-p source))
                         (not (with-current-buffer source (eq proc +emacs-lisp-reduced-flymake-byte-compile--process))))
                     (flymake-log :warning "byte compile process %s is old" proc))
                    ;; if the process exited without problem process the buffer
                    ((zerop (process-exit-status proc))
                     (elisp-flymake--byte-compile-done report-fn source out-buf))
                    ;; otherwise something else horrid has gone wrong and we panic
                    (t (funcall report-fn :panic
                                :explanation
                                (format "byte compile process %s died" proc))))
                 ;; cleanup
                 (ignore-errors (delete-file tmp-file))
                 (kill-buffer out-buf))))))))

(define-minor-mode +emacs-lisp--flymake-non-package-mode
  ""
  :since "3.0.0"
  (if +emacs-lisp--flymake-non-package-mode
      (progn
        (remove-hook! 'flymake-diagnostic-functions :local #'elisp-flymake-checkdoc #'elisp-flymake-byte-compile)
        (add-hook 'flymake-diagnostic-functions #'+emacs-lisp-reduced-flymake-byte-compile nil t))
    (add-hook! 'flymake-diagnostic-functions :local #'elisp-flymake-checkdoc #'elisp-flymake-byte-compile)
    (remove-hook 'flymake-diagnostic-functions #'+emacs-lisp-reduced-flymake-byte-compile t)))

(define-minor-mode +emacs-lisp--flycheck-non-package-mode
  ""
  :since "3.0.0"
  (if (not +emacs-lisp--flycheck-non-package-mode)
      (when (get 'flycheck-disabled-checkers 'initial-value)
        (setq-local flycheck-disabled-checkers (get 'flycheck-disabled-checkers 'initial-value))
        (kill-local-variable 'flycheck-emacs-lisp-check-form))
    (with-memoization (get 'flycheck-disabled-checkers 'initial-value)
      flycheck-disabled-checkers)
    (setq-local flycheck-emacs-lisp-check-form
                (prin1-to-string
                 `(progn
                    (setq doom-modules ',doom-modules
                          doom-disabled-packages ',doom-disabled-packages
                          byte-compile-warnings ',+emacs-lisp-linter-warnings)
                    (condition-case e
                        (progn
                          (require 'doom)
                          (require 'doom-cli)
                          (require 'doom-start))
                      (error
                       (princ
                        (format "%s:%d:%d:Error:Failed to load Doom: %s\n"
                                (or ,(ignore-errors
                                       (file-name-nondirectory
                                        (buffer-file-name (buffer-base-buffer))))
                                    (car command-line-args-left))
                                0 0 (error-message-string e)))))
                    ,(read (default-toplevel-value 'flycheck-emacs-lisp-check-form))))
                flycheck-disabled-checkers (cons 'emacs-lisp-checkdoc
                                                 flycheck-disabled-checkers))))
;;;###autoload
(define-minor-mode +emacs-lisp-non-package-mode
  "Reduce flycheck/flymake verbosity where it is appropriate.

Essentially, this means in any elisp file that either:
- Is not a theme in `custom-theme-load-path',
- Lacks a `provide' statement,
- Lives in a project with a .doommodule file,
- Is a dotfile (like .dir-locals.el or .doomrc).

This generally applies to your private config (`doom-user-dir') or Doom's source
\(`doom-emacs-dir')."
  :since "3.0.0"
  (unless (and (or (bound-and-true-p flycheck-mode)
                   (bound-and-true-p flymake-mode))
               (not (+emacs-lisp--in-package-buffer-p)))
    (setq +emacs-lisp-non-package-mode nil))
  (when (derived-mode-p 'emacs-lisp-mode)
    (add-hook 'after-save-hook #'+emacs-lisp-non-package-mode nil t))
  (let ((toggle (if +emacs-lisp-non-package-mode +1 -1)))
    (cond ((modulep! :checkers syntax +flymake)
           (+emacs-lisp--flymake-non-package-mode toggle))
          ((modulep! :checkers syntax)
           (+emacs-lisp--flycheck-non-package-mode toggle)))))


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


;;
;;; Advice

;;;###autoload
(defun +emacs-lisp--add-doom-elisp-demos-a (fn symbol)
  "Add Doom's own demos to `elisp-demos'.

Intended as :around advice for `elisp-demos--search'."
  (let ((org-inhibit-startup t)
        enable-dir-local-variables
        org-mode-hook)
    (or (funcall fn symbol)
        (with-file-contents! (doom-path doom-docs-dir "examples.org")
          (save-excursion
            (when (re-search-forward
                   (format "^\\*+[ \t]+\\(?:TODO \\)?%s$"
                           (regexp-quote (symbol-name symbol)))
                   nil t)
              (forward-line 1)
              (let ((demos
                     (string-trim
                      (buffer-substring-no-properties
                       (point) (if (re-search-forward "^\\*+ " nil t)
                                   (line-beginning-position)
                                 (point-max))))))
                (unless (string-blank-p demos)
                  demos))))))))

;;;###autoload (put 'map! 'indent-plists-as-data t)
;;;###autoload
(defun +emacs-lisp--calculate-lisp-indent-a (&optional parse-start)
  "Add better indentation for quoted and backquoted lists.

Intended as :override advice for `calculate-lisp-indent'.

Adapted from URL `https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/'."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with
  ;; `defvar` with it's value ommited, marking it special and only defining it
  ;; locally. So if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start)
             (beginning-of-defun))
            ((setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren. Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.  Indent under
                 ;; that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line except the
                      ;; first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      (or
                       ;; Align keywords in plists if each newline begins with
                       ;; a keyword. This is useful for "unquoted plist
                       ;; function" macros, like `map!' and `defhydra'.
                       (when-let ((first (elt state 1))
                                  (char (char-after (1+ first))))
                         (and (eq char ?:)
                              (ignore-errors
                                (or (save-excursion
                                      (goto-char first)
                                      ;; FIXME Can we avoid `syntax-ppss'?
                                      (when-let* ((parse-sexp-ignore-comments t)
                                                  (end (scan-lists (point) 1 0))
                                                  (depth (ppss-depth (syntax-ppss))))
                                        (and (re-search-forward "^\\s-*:" end t)
                                             (= (ppss-depth (syntax-ppss))
                                                (1+ depth)))))
                                    (save-excursion
                                      (cl-loop for pos in (reverse (elt state 9))
                                               unless (memq (char-after (1+ pos)) '(?: ?\())
                                               do (goto-char (1+ pos))
                                               for fn = (read (current-buffer))
                                               if (symbolp fn)
                                               return (function-get fn 'indent-plists-as-data)))))))

                       ;; Check for quotes or backquotes around.
                       (let ((positions (elt state 9))
                             (quotep 0))
                         (while positions
                           (let ((point (pop positions)))
                             (or (when-let (char (char-before point))
                                   (cond
                                    ((eq char ?\())
                                    ((memq char '(?\' ?\`))
                                     (or (save-excursion
                                           (goto-char (1+ point))
                                           (skip-chars-forward "( ")
                                           (when-let (fn (ignore-errors (read (current-buffer))))
                                             (if (and (symbolp fn)
                                                      (fboundp fn)
                                                      ;; Only special forms and
                                                      ;; macros have special
                                                      ;; indent needs.
                                                      (not (functionp fn)))
                                                 (setq quotep 0))))
                                         (cl-incf quotep)))
                                    ((memq char '(?, ?@))
                                     (setq quotep 0))))
                                 ;; If the spelled out `quote' or `backquote'
                                 ;; are used, let's assume
                                 (save-excursion
                                   (goto-char (1+ point))
                                   (and (looking-at-p "\\(\\(?:back\\)?quote\\)[\t\n\f\s]+(")
                                        (cl-incf quotep 2)))
                                 (setq quotep (max 0 (1- quotep))))))
                         (> quotep 0))))
                     ;; Containing sexp has nothing before this line except the
                     ;; first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's almost
                 ;; certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset or
      ;; if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment or it does not apply
                ;; to this argument, try to align a constant-symbol under the
                ;; last preceding constant symbol, if there is such one of the
                ;; last 2 preceding symbols, in the previous uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation where it
                     ;; begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace following an
                       ;; open paren. (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant as
                ;; defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (normal-indent))))))

;; HACK: Quite a few functions here are called often, and so are especially
;;   performance sensitive, so we compile this file on-demand, at least, until
;;   Doom adds a formal compile step to 'doom sync'.
(doom-compile-functions #'+emacs-lisp-highlight-vars-and-faces
                        #'+emacs-lisp-truncate-pin
                        #'+emacs-lisp--calculate-lisp-indent-a)

;;; autoload.el ends here
