;;; lisp/cli/make/completions.el --- generate shell completion scripts -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Variables

;; (defvar doom-make-completions-zsh-spec
;;   '(("FILE" . "_files"))
;;   "TODO")


;;
;;; Commands

(defcli! (make completions)
    ((shell   ("--zsh" "--bash") "Generate a particular flavor of completion files (defaults to $SHELL)")
     ;; TODO (outfile ("-o" "--outfile" file))
     &context context &args args)
  "Generate completion scripts for a Doom-CLI script."
  ;; (unless outfile
  ;;   (user-error "No destination file specified"))
  (let ((shell (or shell (file-name-base (getenv "SHELL"))))
        ;; TODO Allow this command to read other Doom binscripts, which will
        ;;   dump their `doom-cli--table' if __DOOMDUMP is set.
        ;; (table (read (letenv! (("__DOOMDUMP" "1")) (apply #'sh! script-file args))))
        )
    (print!
     "%s" (pcase (string-remove-prefix "--" shell)
            ("zsh"  (doom-make-completions-zsh  context nil))
            ("bash" (doom-make-completions-bash context nil))
            (_ (user-error "No support for %S shell at this time" shell))))))


;;
;;; ZSH Helpers

;; TODO Write to OUTFILE when specified
(defun doom-make-completions-zsh (context _outfile)
  (let* ((cli (doom-cli-get context))
         (prefix (doom-cli-context-prefix context))
         (options (doom-cli-help--options cli t))
         (commands (doom-cli-subcommands (list prefix))))
    (with-temp-buffer
      (insert "#compdef " (doom-cli-context-prefix context) "\n\n"
              "_globalargs=(\n  ")
      (doom-make-completions--zsh-insert-options
       (append '(((("--help") ("-?")) . "Show help documentation")
                 ((("--version")) . "Show version information"))
               (alist-get 'global options))
       "\n  ")
      (insert "\n)\n\n")
      (doom-make-completions--zsh-insert-command '("doom"))
      (mapc #'doom-make-completions--zsh-insert-command commands)
      ;; (insert "\n\n_doom")
      (buffer-string))
    ;; (set-file-modes outfile #o755)
    ;; outfile
    ))

(defun doom-make-completions--zsh-insert-options (options &optional cr)
  ;; FIXME Refactor, generalize, and parameterize this mess
  (dolist (option options)
    (let* ((switches (cl-loop for (sw . args) in (car option)
                              if (string-prefix-p "--[no-]" sw)
                              collect (cons (concat "--" (string-remove-prefix "--[no-]" sw)) args)
                              and collect (cons (concat "--no-" (string-remove-prefix "--[no-]" sw)) args)
                              else collect (cons sw args)))
           (args (remove "..." (cdr (car switches))))
           (argspec (cl-loop for arg in args
                             concat
                             (format ":%s:%s"
                                     (replace-regexp-in-string
                                      ":" ";" (shell-quote-argument arg))
                                     (or (plist-get (cdr (assoc (intern-soft (downcase (car args)))
                                                                doom-cli-option-arg-types))
                                                    :zshcomp)
                                         ""))))
           (multiple? (member "..." (cdr (car switches)))))
      (insert (format "%s%s%s"
                      (if multiple?
                          "\\*"
                        (format "'(%s)'" (mapconcat #'car switches " ")))
                      (if (cdr switches)
                          (format
                           "{%s}" (combine-and-quote-strings
                                   (cl-loop for (sw . _) in switches
                                            if (and args (string-prefix-p "--" sw))
                                            collect (concat (shell-quote-argument sw) "=")
                                            else collect (shell-quote-argument sw))
                                   ","))
                        (format "%s%s" (caar switches)
                                (if (and args (string-prefix-p "--" (caar switches)))
                                    "=" "")))
                      (format "'[%s]%s'"
                              (replace-regexp-in-string "'" "''" (cdr option))
                              (or argspec "")))
              (or cr "\n")))))

(defun doom-make-completions--zsh-insert-command (command)
  (let* ((commandstr (doom-cli-command-string command))
         (options (alist-get 'local (doom-cli-help--options (doom-cli-get command) t)))
         (subcommands (doom-cli-subcommands command 1)))
    (insert "_" (replace-regexp-in-string "[- ]" "_" commandstr) "() {\n"
            "  local line state\n"
            "  _arguments -s -S -C \"${_globalargs[@]}\" \\\n                ")
    (doom-make-completions--zsh-insert-options options " \\\n                ")
    (insert "\"1: :->cmds\" \"*::arg:->args\"\n"
            "  case $state in\n"
            "    cmds)\n"
            "      _values \"" commandstr "\" \\\n          "
            (string-join
             (cl-loop for command in subcommands
                      unless (string-prefix-p ":" (car command))
                      collect (format "'%s[%s]' "
                                      (car (last command))
                                      (or (doom-cli-short-docs (doom-cli-get command))
                                          "TODO")))
             " \\\n          ")
            "\n      ;;\n"
            "    args)\n"
            "      case $line[1] in\n          "
            (string-join
             (cl-loop for command in subcommands
                      unless (string-prefix-p ":" (car command))
                      collect (format "%s) _%s ;; "
                                      (car (last command))
                                      (replace-regexp-in-string "[- ]" "_" (doom-cli-command-string command))))
             "\n          ")
            "\n      esac\n"
            "      ;;\n"
            "  esac\n"
            "}\n")))


;;
;;; Bash helpers

(defun doom-make-completions-bash (context _outfile)
  (user-error "Bash completion exporter hasn't been implemented yet!"))

;;; completions.el ends here
