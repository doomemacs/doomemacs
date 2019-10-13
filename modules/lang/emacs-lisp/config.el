;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;;* [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp buffers.")


;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)


;;
;;; Config

(use-package! elisp-mode
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :config
  (set-repl-handler! 'emacs-lisp-mode #'+emacs-lisp/open-repl)
  (set-eval-handler! 'emacs-lisp-mode #'+emacs-lisp-eval)
  (set-lookup-handlers! 'emacs-lisp-mode
    :definition    #'elisp-def
    :documentation #'+emacs-lisp-lookup-documentation)
  (set-docsets! 'emacs-lisp-mode "Emacs Lisp")
  (set-pretty-symbols! 'emacs-lisp-mode :lambda "lambda")
  (set-rotate-patterns! 'emacs-lisp-mode
    :symbols '(("t" "nil")
               ("let" "let*")
               ("when" "unless")
               ("advice-add" "advice-remove")
               ("add-hook" "remove-hook")
               ("add-hook!" "remove-hook!")
               ("it" "xit")
               ("describe" "xdescribe")))

  (setq-hook! 'emacs-lisp-mode-hook
    tab-width (or lisp-indent-offset 2)
    ;; shorter name in modeline
    mode-name "Elisp"
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp +emacs-lisp-outline-regexp)

  ;; variable-width indentation is superior in elisp
  (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode nil #'eq)

  ;; Use helpful instead of describe-* from `company'
  (advice-add #'elisp--company-doc-buffer :around #'doom-use-helpful-a)

  (add-hook! 'emacs-lisp-mode-hook
             #'outline-minor-mode
             ;; fontificiation
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode
             ;; initialization
             #'+emacs-lisp-extend-imenu-h)

  ;; Flycheck's two emacs-lisp checkers produce a *lot* of false positives in
  ;; emacs configs, so we disable `emacs-lisp-checkdoc' and reduce the
  ;; `emacs-lisp' checker's verbosity.
  (add-hook 'flycheck-mode-hook #'+emacs-lisp-reduce-flycheck-errors-in-emacs-config-h)

  ;; Special syntax highlighting for elisp...
  (font-lock-add-keywords
   'emacs-lisp-mode
   (append `(;; custom Doom cookies
             ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
           ;; highlight defined, special variables & functions
           (when +emacs-lisp-enable-extra-fontification
             `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

  ;; Recenter window after following definition
  (advice-add #'elisp-def :after #'doom-recenter-a)

  (map! :localleader
        :map emacs-lisp-mode-map
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("d" . "debug")
          "f" #'+emacs-lisp/edebug-instrument-defun-on
          "F" #'+emacs-lisp/edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
          "b" #'eval-buffer
          "d" #'eval-defun
          "e" #'eval-last-sexp
          "r" #'eval-region)
        (:prefix ("g" . "goto")
          "f" #'find-function
          "v" #'find-variable)))

(use-package! ielm
  :config
  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/
  (require 'dash)

  (defun doom--kwd->comint-kwd (kwd)
    "Converts a `font-lock-keywords' KWD for `comint-mode' input fontification."
    (-let (((matcher . match-highlights) kwd))
      ;; below is ` quoted but breaks my blogs syntax higlighting, so removing it!
      ;; make sure to capture first paren in a ` if copying!
      `((lambda (limit)
          ;; Matcher can be a function or a regex
          (when ,(if (symbolp matcher)
                     `(,matcher limit)
                   `(re-search-forward ,matcher limit t))
            ;; While the SUBEXP can be anything, this search always can use zero
            (-let ((start (match-beginning 0))
                   ((comint-last-start . comint-last-end) comint-last-prompt)
                   (state (syntax-ppss)))
              (and (> start comint-last-start)
                   ;; Make sure not in comment or string
                   ;; have to manually do this in custom MATCHERs
                   (not (or (nth 3 state) (nth 4 state)))))))
        ,@match-highlights)))

  (setq doom--ielm-font-lock-kwds
        `(,@(-map #'doom--kwd->comint-kwd lisp-el-font-lock-keywords-2)
          ,@(-map #'doom--kwd->comint-kwd lisp-cl-font-lock-keywords-2)))

  (add-hook! 'ielm-mode-hook
    (defun doom--set-ielm-kwds-h ()
      (interactive)
      (message "running my hook boy")
      (setq-local font-lock-keywords doom--ielm-font-lock-kwds))))
;;
;;; Packages

(map! :when (featurep! :editor evil)
      :after macrostep
      :map macrostep-keymap
      :n [return] #'macrostep-expand)


;;;###package overseer
(autoload 'overseer-test "overseer" nil t)
(remove-hook 'emacs-lisp-mode-hook 'overseer-enable-mode)


(use-package! flycheck-cask
  :when (featurep! :tools flycheck)
  :defer t
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t)))


(use-package! elisp-demos
  :defer t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  :config
  (defadvice! +emacs-lisp--add-doom-elisp-demos-a (orig-fn symbol)
    "Add Doom's own demos to help buffers."
    :around #'elisp-demos--search
    (or (funcall orig-fn symbol)
        (when-let (demos-file (doom-glob doom-docs-dir "api.org"))
          (with-temp-buffer
            (insert-file-contents demos-file)
            (goto-char (point-min))
            (when (re-search-forward
                   (format "^\\*\\*\\* %s$" (regexp-quote (symbol-name symbol)))
                   nil t)
              (let (beg end)
                (forward-line 1)
                (setq beg (point))
                (if (re-search-forward "^\\*" nil t)
                    (setq end (line-beginning-position))
                  (setq end (point-max)))
                (string-trim (buffer-substring-no-properties beg end)))))))))


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
  (map! :map buttercup-minor-mode-map
        :localleader
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
