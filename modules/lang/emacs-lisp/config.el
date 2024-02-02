;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp buffers.")

(defvar +emacs-lisp-linter-warnings
  '(not free-vars    ; don't complain about unknown variables
        noruntime    ; don't complain about unknown function calls
        unresolved)  ; don't complain about undefined functions
  "The value for `byte-compile-warnings' in non-packages.

This reduces the verbosity of flycheck in Emacs configs and scripts, which are
so stateful that the deluge of false positives (from the byte-compiler,
package-lint, and checkdoc) can be more overwhelming than helpful.

See `+emacs-lisp-non-package-mode' for details.")


;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)


;;
;;; Config

(use-package! elisp-mode
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :interpreter ("doomscript" . emacs-lisp-mode)
  :config
  (set-repl-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+emacs-lisp/open-repl)
  (set-eval-handler! '(emacs-lisp-mode lisp-interaction-mode) #'+emacs-lisp-eval)
  (set-lookup-handlers! '(emacs-lisp-mode lisp-interaction-mode helpful-mode)
    :definition    #'+emacs-lisp-lookup-definition
    :documentation #'+emacs-lisp-lookup-documentation)
  (set-docsets! '(emacs-lisp-mode lisp-interaction-mode) "Emacs Lisp")
  (set-ligatures! 'emacs-lisp-mode :lambda "lambda")
  (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(emacs-lisp-mode))
  (set-rotate-patterns! 'emacs-lisp-mode
    :symbols '(("t" "nil")
               ("let" "let*")
               ("when" "unless")
               ("advice-add" "advice-remove")
               ("defadvice!" "undefadvice!")
               ("add-hook" "remove-hook")
               ("add-hook!" "remove-hook!")
               ("it" "xit")
               ("describe" "xdescribe")))

  (setq-hook! 'emacs-lisp-mode-hook
    ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
    ;; with a tab width of 8. Any smaller and the indentation will be
    ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
    ;; safe to ignore this setting otherwise.
    tab-width 8
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp +emacs-lisp-outline-regexp
    outline-level #'+emacs-lisp-outline-level)

  ;; DEPRECATED: Remove when 27.x support is dropped.
  (when (< emacs-major-version 28)
    ;; As of Emacs 28+, `emacs-lisp-mode' uses a shorter label in the mode-line
    ;; ("ELisp/X", where X = l or d, depending on `lexical-binding'). In <=27,
    ;; it uses "Emacs-Lisp". The former is more useful, so I backport it:
    (setq-hook! 'emacs-lisp-mode-hook
      mode-name `("ELisp"
                  (lexical-binding (:propertize "/l"
                                    help-echo "Using lexical-binding mode")
                                   (:propertize "/d"
                                    help-echo "Using old dynamic scoping mode"
                                    face warning
                                    mouse-face mode-line-highlight)))))

  ;; Fixed indenter that intends plists sensibly.
  (advice-add #'calculate-lisp-indent :override #'+emacs-lisp--calculate-lisp-indent-a)

  ;; variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
  ;; and `editorconfig' would force fixed indentation on elisp.
  (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode)

  (add-hook! 'emacs-lisp-mode-hook
             ;; Allow folding of outlines in comments
             #'outline-minor-mode
             ;; Make parenthesis depth easier to distinguish at a glance
             #'rainbow-delimiters-mode
             ;; Make quoted symbols easier to distinguish from free variables
             #'highlight-quoted-mode
             ;; Extend imenu support to Doom constructs
             #'+emacs-lisp-extend-imenu-h
             ;; Ensure straight sees modifications to installed packages
             #'+emacs-lisp-init-straight-maybe-h)

  ;; UX: Both Flycheck's and Flymake's two emacs-lisp checkers produce a *lot*
  ;;    of false positives in non-packages (like Emacs configs or elisp
  ;;    scripts), so I disable `checkdoc' (`emacs-lisp-checkdoc',
  ;;    `elisp-flymake-checkdoc') and set `byte-compile-warnings' to a subset
  ;;    that makes more sense (see `+emacs-lisp-linter-warnings')
  (add-hook! '(flycheck-mode-hook flymake-mode-hook) #'+emacs-lisp-non-package-mode)

  (defadvice! +syntax--fix-elisp-flymake-load-path (orig-fn &rest args)
    "Set load path for elisp byte compilation Flymake backend"
    :around #'elisp-flymake-byte-compile
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply orig-fn args)))

  ;; Enhance elisp syntax highlighting, by highlighting Doom-specific
  ;; constructs, defined symbols, and truncating :pin's in `package!' calls.
  (font-lock-add-keywords
   'emacs-lisp-mode
   (append `(;; custom Doom cookies
             ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
           ;; Shorten the :pin of `package!' statements to 10 characters
           `(("(package!\\_>" (0 (+emacs-lisp-truncate-pin))))
           ;; highlight defined, special variables & functions
           (when +emacs-lisp-enable-extra-fontification
             `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))
 
  (defadvice! +emacs-lisp-append-value-to-eldoc-a (fn sym)
    "Display variable value next to documentation in eldoc."
    :around #'elisp-get-var-docstring
    (when-let (ret (funcall fn sym))
      (if (boundp sym)
          (concat ret " "
                  (let* ((truncated " [...]")
                         (print-escape-newlines t)
                         (str (symbol-value sym))
                         (str (prin1-to-string str))
                         (limit (- (frame-width) (length ret) (length truncated) 1)))
                    (format (format "%%0.%ds%%s" (max limit 0))
                            (propertize str 'face 'warning)
                            (if (< (length str) limit) "" truncated))))
        ret)))

  (map! :localleader
        :map (emacs-lisp-mode-map lisp-interaction-mode-map)
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("d" . "debug")
          "f" #'+emacs-lisp/edebug-instrument-defun-on
          "F" #'+emacs-lisp/edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
          "b" #'eval-buffer
          "d" #'eval-defun
          "e" #'eval-last-sexp
          "r" #'eval-region
          "l" #'load-library)
        (:prefix ("g" . "goto")
          "f" #'find-function
          "v" #'find-variable
          "l" #'find-library)))

(use-package! ielm
  :defer t
  :config
  (set-lookup-handlers! 'inferior-emacs-lisp-mode
    :definition    #'+emacs-lisp-lookup-definition
    :documentation #'+emacs-lisp-lookup-documentation)

  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  (setq ielm-font-lock-keywords
        (append '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
                   (1 font-lock-comment-face)
                   (2 font-lock-constant-face)))
                (when (require 'highlight-numbers nil t)
                  (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode))
                (cl-loop for (matcher . match-highlights)
                         in (append lisp-el-font-lock-keywords-2
                                    lisp-cl-font-lock-keywords-2)
                         collect
                         `((lambda (limit)
                             (when ,(if (symbolp matcher)
                                        `(,matcher limit)
                                      `(re-search-forward ,matcher limit t))
                               ;; Only highlight matches after the prompt
                               (> (match-beginning 0) (car comint-last-prompt))
                               ;; Make sure we're not in a comment or string
                               (let ((state (syntax-ppss)))
                                 (not (or (nth 3 state)
                                          (nth 4 state))))))
                           ,@match-highlights)))))


;;
;;; Packages

;;;###package overseer
(autoload 'overseer-test "overseer" nil t)
;; Properly lazy load overseer by not loading it so early:
(remove-hook 'emacs-lisp-mode-hook #'overseer-enable-mode)


(use-package! flycheck-cask
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :defer t
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t)))


(use-package! flycheck-package
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :after flycheck
  :config (flycheck-package-setup))


(use-package! elisp-demos
  :defer t
  :init
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update)
  :config
  (advice-add #'elisp-demos--search :around #'+emacs-lisp--add-doom-elisp-demos-a))


(use-package! buttercup
  :defer t
  :minor ("/test[/-].+\\.el$" . buttercup-minor-mode)
  :preface
  ;; buttercup.el doesn't define a keymap for `buttercup-minor-mode', as we have
  ;; to fool its internal `define-minor-mode' call into thinking one exists, so
  ;; it will associate it with the mode.
  (defvar buttercup-minor-mode-map (make-sparse-keymap))
  :config
  (set-popup-rule! "^\\*Buttercup\\*$" :size 0.45 :select nil :ttl 0)
  (set-yas-minor-mode! 'buttercup-minor-mode)
  (when (featurep 'evil)
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map buttercup-minor-mode-map
        :prefix "t"
        "t" #'+emacs-lisp/buttercup-run-file
        "a" #'+emacs-lisp/buttercup-run-project
        "s" #'buttercup-run-at-point))


;;
;;; Project modes

(def-project-mode! +emacs-lisp-ert-mode
  :modes '(emacs-lisp-mode)
  :match "/test[/-].+\\.el$"
  :add-hooks '(overseer-enable-mode))
