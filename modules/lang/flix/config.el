;;; lang/flix/config.el -*- lexical-binding: t; -*-

(defconst flix-mode-keywords
  '("alias" "and" "as" "case" "catch" "chan" "choose" "class" "def"
    "default" "deref" "else" "enum" "exists" "false" "forall" "force"
    "from" "get" "if" "import" "inline" "instance" "into" "lat" "law"
    "lawless" "lazy" "let" "match" "matchEff" "mut" "namespace" "new"
    "not" "opaque" "or" "override" "project" "pub" "query" "ref"
    "reifyBool" "reifyEff" "reifyType" "rel" "scoped" "sealed" "select"
    "set" "solve" "spawn" "true" "try" "type" "unlawful" "use" "where"
    "with")
  "Keywords recognized by `flix-mode'.")

(defvar flix-mode-font-lock-keywords
  `(("\\_<Impure\\|null\\_>\\|\\?\\?\\?\\|\\?[_[:lower:]][_[:alnum:]]*" (0 font-lock-warning-face))
    ("\\_<Pure\\_>" (0 font-lock-function-name-face))
    ("\\_<\\(true\\|false\\)\\_>" (0 font-lock-builtin-face))
    ("\\<\\(\\sw+\\)(" (1 font-lock-function-name-face))
    ("let\\*?[ \t]+\\([_[:lower:]][_[:alnum:]]*\\)" (1 font-lock-variable-name-face))
    ("\\_<\\([_[:lower:]][_[:alnum:]]*\\)[ \t]*:[ \t_[:upper:]]" (1 font-lock-variable-name-face))
    ("\\_<\\([_[:upper:]][_[:alnum:]]*\\)\\_>" (0 font-lock-type-face))
    (,(concat "\\_<" (regexp-opt flix-mode-keywords) "\\_>") (0 font-lock-keyword-face)))
  "Keyword highlighting for `flix-mode'.")

(defvar flix-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `flix-mode'.")

(defcustom flix-mode-tab-width 4
  "The tab width to use for indentation.")

(defun flix-mode--indent-further ()
  (interactive)
  (let ((new-indent (+ (current-indentation) flix-mode-tab-width)))
    (if (> (current-column) (current-indentation))
        (save-excursion (indent-line-to new-indent))
      (indent-line-to new-indent))))

(defun flix-mode--indent-less ()
  (interactive)
  (save-excursion
    (indent-line-to (max 0 (- (current-indentation) flix-mode-tab-width)))))

(defun flix-mode--newline-and-maybe-indent ()
  (interactive)
  (let ((indent-further (and (eolp) (looking-back "[{(=]")))
        (prev-indent (current-indentation)))
    (newline)
    (if indent-further
        (indent-line-to (+ prev-indent flix-mode-tab-width))
      (indent-line-to prev-indent))))

(defvar flix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'flix-mode--newline-and-maybe-indent)
    (define-key map [return] 'flix-mode--newline-and-maybe-indent)
    (define-key map "\C-j" 'flix-mode--newline-and-maybe-indent)
    (define-key map [tab] 'flix-mode--indent-further)
    (define-key map [backtab] 'flix-mode--indent-less)
    map)
  "Keymap for `flix-mode'.")

;;;###autoload
(define-derived-mode flix-mode prog-mode "Flix"
  "A major mode for editing Flix files."
  :syntax-table flix-mode-syntax-table
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "//")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\) *")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(flix-mode-font-lock-keywords))
  (add-to-list 'electric-indent-chars ?\}))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.flix\\'" . flix-mode))

(provide 'flix-mode)

;;; flix-mode.el ends here


;;; Flix Repl
(require 'comint)
(require 'ansi-color)

(defvar flix-jar-directory nil
  "The directory from which to start the Flix REPL.")

(defvar flix-last-buffer nil
  "The last flix buffer to call the repl")

(defun +flix--repl-apply-ansi-color (string)
  "Apply ANSI color codes to STRING."
  (ansi-color-apply string))

(defun +flix--repl-stop-repl ()
  "Stop the current Flix REPL process, if any."
  (let ((buffer (get-buffer "*Flix REPL*")))
    (when (and buffer (get-buffer-process buffer))
      (kill-process (get-buffer-process buffer))
      (kill-buffer buffer))))

(defun +flix--repl-start-repl ()
  "Start a new Flix REPL process."
  (let* ((default-directory (or flix-jar-directory (read-directory-name "Select directory with flix.jar: ")))
         (buffer (get-buffer-create "*Flix REPL*")))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (setq flix-jar-directory default-directory)
        (apply 'make-comint-in-buffer "Flix REPL" buffer "java" nil '("-jar" "flix.jar" "repl"))
        (flix-repl-mode)))
    (pop-to-buffer buffer)))

(defvar flix-repl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `flix-repl-mode`.")

(defun +flix--repl-initialize ()
  "Helper function to initialize `flix-repl`."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (add-hook 'comint-preoutput-filter-functions '+flix--repl-apply-ansi-color nil t))

(define-derived-mode flix-repl-mode comint-mode "Flix REPL"
  "Major mode for `flix-repl`."
  nil "Flix REPL"
  (setq comint-prompt-regexp "flix> ")
  (setq mode-line-process '(":%s"))
  (setq comint-get-old-input (lambda () ""))
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (set (make-local-variable 'font-lock-defaults) '(nil t))
  (set (make-local-variable 'comint-input-filter)
       (lambda (str) (not (string-match "\\`\\s-*\\'" str))))
  (set (make-local-variable 'comint-output-filter-functions)
       (list 'comint-postoutput-scroll-to-bottom)))

(add-hook 'flix-repl-mode-hook '+flix--repl-initialize)

(provide 'flix-repl)


;; Keymaps for flix-mode.
(map! :map 'flix-mode-map
      :desc "restart flix repl" "C-c C-c C-r" #'+flix/repl-restart
      :desc "set jar directory" "C-c C-d" #'+flix/set-jar-directory
      :desc "run flix project" "C-c C-r" #'+flix/run-project
      :desc "build flix project" "C-c C-b" #'+flix/build-project
      :desc "flix install jar" "C-c C-j" #'+flix/install-jar
      :desc "flix init project" "C-c C-i" #'+flix/init-project
      :desc "flix command" "C-c C-SPC" #'+flix/flix-command
      :desc "goto flix repl" "C-c C-z" #'+flix/goto-repl)

;; Keymaps for flix-repl-mode.
(map! :map 'flix-repl-mode-map
      :desc "restart flix repl" "C-c C-c C-r" #'+flix/repl-restart
      :desc "goto flix buffer" "C-c C-z" #'+flix/goto-flix-buffer)
