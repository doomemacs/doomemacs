;;; lisp/cli/help.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; This file defines special commands that the Doom CLI will invoke when a
;; command is passed with -?, --help, or --version. They can also be aliased to
;; a sub-command to make more of its capabilities accessible to users, with:
;;
;;    (defcli-alias! (myscript (help h)) (:help))
;;
;; You can define your own command-specific help handlers, e.g.
;;
;;    (defcli! (:help myscript subcommand) () ...)
;;
;; And it will be invoked instead of the generic one.
;;
;;; Code:

;;
;;; Variables

(defvar doom-help-commands '("%p %c {-?,--help}")
  "A list of help commands recognized for the running script.

Recognizes %p (for the prefix) and %c (for the active command).")


;;
;;; Commands

(defcli! (:root :help)
    ((localonly? ("-g" "--no-global") "Hide global options")
     (manpage?   ("--manpage")   "Generate in manpage format")
     (commands?  ("--commands")  "List all known commands")
     &multiple
     (sections   ("--synopsis" "--subcommands" "--similar" "--envvars"
                  "--postamble")
                 "Show only the specified sections.")
     &context context
     &args command)
  "Show documentation for a Doom CLI command.

OPTIONS:
  --synopsis, --subcommands, --similar, --envvars, --postamble
    TODO"
  (doom-cli-load-all)
  (when (doom-cli-context-error context)
    (terpri))
  (let* ((command (cons (doom-cli-context-prefix context) command))
         (cli (doom-cli-get command t))
         (rcli (doom-cli-get cli))
         (fallbackcli (cl-loop with targets = (doom-cli--command-expand (butlast command) t)
                               for cmd in (cons command targets)
                               if (doom-cli-get cmd t)
                               return it)))
    (cond (commands?
           (let ((cli (or cli (doom-cli-get (doom-cli-context-prefix context)))))
             (print! "Commands under '%s':\n%s"
                     (doom-cli-command-string cli)
                     (indent (doom-cli-help--render-commands
                              (or (doom-cli-subcommands cli)
                                  (user-error "No commands found"))
                              :prefix (doom-cli-command cli)
                              :inline? t
                              :docs? t)))))
          ((null sections)
           (if (null cli)
               (signal 'doom-cli-command-not-found-error command)
             (doom-cli-help--print cli context manpage? localonly?)
             (exit! :pager?)))
          ((dolist (section sections)
             (unless (equal section (car sections)) (terpri))
             (pcase section
               ("--synopsis"
                (print! "%s" (doom-cli-help--render-synopsis
                              (doom-cli-help--synopsis cli)
                              "Usage: ")))
               ("--subcommands"
                (print! "%s\n%s" (bold "Available commands:")
                        (indent (doom-cli-help--render-commands
                                 (doom-cli-subcommands rcli 1)
                                 :prefix command
                                 :grouped? t
                                 :docs? t)
                                doom-print-indent-increment)))
               ("--similar"
                (unless command
                  (user-error "No command specified"))
                (let ((similar (doom-cli-help-similar-commands command 0.4)))
                  (print! "Similar commands:")
                  (if (not similar)
                      (print! (indent (warn "Can't find any!")))
                    (dolist (command (seq-take similar 10))
                      (print! (indent (item "(%d%%) %s"))
                              (* (car command) 100)
                              (doom-cli-command-string (cdr command)))))))
               ("--envvars"
                (let* ((key "ENVIRONMENT VARIABLES")
                       (clis (if command (doom-cli-find command) (hash-table-values doom-cli--table)))
                       (clis (seq-remove #'doom-cli-alias clis))
                       (clis (seq-filter (fn! (cdr (assoc key (doom-cli-docs %)))) clis))
                       (clis (seq-group-by #'doom-cli-command clis)))
                  (print! "List of environment variables for %s:\n" command)
                  (if (null clis)
                      (print! (indent "None!"))
                    (dolist (group clis)
                      (print! (bold "%s%s:"
                                    (doom-cli-command-string (car group))
                                    (if (doom-cli-fn (doom-cli-get (car group)))
                                        "" " *")))
                      (dolist (cli (cdr group))
                        (print! (indent "%s") (markup (cdr (assoc key (doom-cli-docs cli))))))))))
               ("--postamble"
                (print! "See %s for documentation."
                        (join (cl-loop with spec =
                                       `((?p . ,(doom-cli-context-prefix context))
                                         (?c . ,(doom-cli-command-string (cdr (doom-cli-command (or cli fallbackcli))))))
                                       for cmd in doom-help-commands
                                       for formatted = (trim (format-spec cmd spec))
                                       collect (replace-regexp-in-string
                                                " +" " " (format "'%s'" formatted)))
                              " or ")))))))))

(defcli! (:root :version)
    ((simple? ("--simple"))
     &context context)
  "Show installed versions of Doom, Doom modules, and Emacs."
  (doom/version)
  (unless simple?
    (terpri)
    (with-temp-buffer
      (insert-file-contents (doom-path doom-emacs-dir "LICENSE"))
      (re-search-forward "^Copyright (c) ")
      (print! "%s\n" (trim (thing-at-point 'line t)))
      (print! (p "Doom Emacs uses the MIT license and is provided without warranty "
                 "of any kind. You may redistribute and modify copies if "
                 "given proper attribution. See the LICENSE file for details.")))))


;;
;;; Helpers

(defun doom-cli-help (cli)
  "Return an alist of documentation summarizing CLI (a `doom-cli')."
  (let* ((rcli (doom-cli-get cli))
         (docs (doom-cli-docs rcli)))
    `((command     . ,(doom-cli-command-string cli))
      (summary     . ,(or (cdr (assoc "SUMMARY" docs)) "TODO"))
      (description . ,(or (cdr (assoc "MAIN" docs)) "TODO"))
      (synopsis    . ,(doom-cli-help--synopsis cli))
      (arguments   . ,(doom-cli-help--arguments rcli))
      (options     . ,(doom-cli-help--options rcli))
      (commands    . ,(doom-cli-subcommands cli 1))
      (sections    . ,(seq-filter #'cdr (cddr docs))))))

(defun doom-cli-help-similar-commands (command &optional maxscore)
  "Return N commands that are similar to COMMAND."
  (seq-take-while
   (fn! (>= (car %) (or maxscore 0.0)))
   (seq-sort-by
    #'car #'>
    (cl-loop with prefix = (seq-find #'doom-cli-get (nreverse (doom-cli--command-expand command t)))
             with input = (doom-cli-command-string (cdr (doom-cli--command command t)))
             for command in (hash-table-keys doom-cli--table)
             if (doom-cli-fn (doom-cli-get command))
             if (equal prefix (seq-take command (length prefix)))
             collect (cons (doom-cli-help--similarity
                            input (doom-cli-command-string (cdr command)))
                           command)))))

(defun doom-cli-help--similarity (s1 s2)
  ;; Ratcliff-Obershelp similarity
  (let* ((s1 (downcase s1))
         (s2 (downcase s2))
         (s1len (length s1))
         (s2len (length s2)))
    (if (or (zerop s1len)
            (zerop s2len))
        0.0
      (/ (let ((i 0) (j 0) (score 0) jlast)
           (while (< i s1len)
             (unless jlast (setq jlast j))
             (if (and (< j s2len)
                      (= (aref s1 i) (aref s2 j)))
                 (progn (cl-incf score)
                        (cl-incf i)
                        (cl-incf j))
               (setq m 0)
               (cl-incf j)
               (when (>= j s2len)
                 (setq j (or jlast j)
                       jlast nil)
                 (cl-incf i))))
           (* 2.0 score))
         (+ (length s1)
            (length s2))))))

;;; Help: printers
;; TODO Parameterize optional args with `cl-defun'
(defun doom-cli-help--print (cli context &optional manpage? noglobal?)
  "Write CLI's documentation in a manpage-esque format to stdout."
  (let-alist (doom-cli-help cli)
    (let* ((alist
            `(,@(if manpage?
                    `((nil    . ,(let* ((title (cadr (member "--load" command-line-args)))
                                        (width (floor (/ (- (doom-cli-context-width context)
                                                            (length title))
                                                         2.0))))
                                   ;; FIXME Who am I fooling?
                                   (format (format "%%-%ds%%s%%%ds" width width)
                                           "DOOM(1)" title "DOOM(1)")))
                      ("NAME" . ,(concat .command " - " .summary))
                      ("SYNOPSIS" . ,(doom-cli-help--render-synopsis .synopsis nil t))
                      ("DESCRIPTION" . ,.description))
                  `((nil . ,(doom-cli-help--render-synopsis .synopsis "Usage: "))
                    (nil . ,(string-join (seq-remove #'string-empty-p (list .summary .description))
                                         "\n\n"))))
              ("ARGUMENTS" . ,(doom-cli-help--render-arguments .arguments))
              ("COMMANDS"
               . ,(doom-cli-help--render-commands
                   .commands :prefix (doom-cli-command cli) :grouped? t :docs? t))
              ("OPTIONS"
               . ,(doom-cli-help--render-options
                   (if (or (not (doom-cli-fn cli)) noglobal?)
                       `(,(assq 'local .options))
                     .options)
                   cli))))
           (command (doom-cli-command cli)))
      (letf! (defun printsection (section)
               (print! "%s\n"
                       (if (null section)
                           (dark "TODO")
                         (markup
                          (format-spec
                           section `((?p . ,(car command))
                                     (?c . ,(doom-cli-command-string (cdr command))))
                           'ignore)))))
        (pcase-dolist (`(,label . ,contents) alist)
          (when (and contents (not (string-blank-p contents)))
            (when label
              (print! (bold "%s%s") label (if manpage? "" ":")))
            (print-group! :if label (printsection contents))))
        (pcase-dolist (`(,label . ,contents) .sections)
          (when (and contents (not (assoc label alist)))
            (print! (bold "%s:") label)
            (print-group! (printsection contents))))))))

;;; Help: synopsis
(defun doom-cli-help--synopsis (cli &optional all-options?)
  (let* ((rcli (doom-cli-get cli))
         (opts (doom-cli-help--options rcli))
         (opts (mapcar #'car (if all-options? (mapcan #'cdr opts) (alist-get 'local opts))))
         (opts (cl-loop for opt in opts
                        for args = (cdar opt)
                        for switches = (mapcar #'car opt)
                        for multi? = (member "..." args)
                        if args
                        collect (format (if multi? "[%s %s]..." "[%s %s]")
                                        (string-join switches "|")
                                        (string-join (remove "..." args) "|"))
                        else collect (format "[%s]" (string-join switches "|"))))
         (args (doom-cli-arguments rcli))
         (subcommands? (doom-cli-subcommands rcli 1 :predicate? t)))
    `((command  . ,(doom-cli-command cli))
      (options  ,@opts)
      (required ,@(mapcar (fn! (upcase (format "`%s'" %))) (if subcommands? '(command) (alist-get '&required args))))
      (optional ,@(mapcar (fn! (upcase (format "[`%s']" %)))(alist-get '&optional args)))
      (rest     ,@(mapcar (fn! (upcase (format "[`%s'...]" %))) (if subcommands? '(args) (alist-get '&args args)))))))

(defun doom-cli-help--render-synopsis (synopsis &optional prefix)
  (let-alist synopsis
    (let ((doom-print-indent 0)
          (prefix (or prefix ""))
          (command (doom-cli-command-string .command)))
      (string-trim-right
       (format! "%s\n\n"
                (fill (concat (bold prefix)
                              (format "%s " command)
                              (markup
                               (join (append .options
                                             (and .options
                                                  (or .required
                                                      .optional
                                                      .rest)
                                                  (list (dark "[--]")))
                                             .required
                                             .optional
                                             .rest))))
                      80 (1+ (length (concat prefix command)))))))))

;;; Help: arguments
(defun doom-cli-help--arguments (cli &optional all?)
  (doom-cli-help--parse-docs (doom-cli-find cli t) "ARGUMENTS"))

(defun doom-cli-help--render-arguments (arguments)
  (mapconcat (lambda (arg)
               (format! "%-20s\n%s"
                        (underscore (car arg))
                        (indent (if (equal (cdr arg) "TODO")
                                    (dark (cdr arg))
                                  (cdr arg))
                                doom-print-indent-increment)))
             arguments
             "\n"))

;;; Help: commands
(cl-defun doom-cli-help--render-commands (commands &key prefix grouped? docs? (inline? t))
  (with-temp-buffer
    (let* ((doom-print-indent 0)
           (commands (seq-group-by (fn! (if grouped? (doom-cli-prop (doom-cli-get % t) :group)))
                                   (nreverse commands)))
           (toplevel (assq nil commands))
           (rest (remove toplevel commands))
           (drop (if prefix (length prefix) 0))
           (minwidth
            (apply
             #'max (or (cl-loop for cmd in (apply #'append (mapcar #'cdr commands))
                                for cmd = (seq-drop cmd drop)
                                collect (length (doom-cli-command-string cmd)))
                       (list 15))))
           (ellipsis (doom-print--style 'dark " […]"))
           (ellipsislen (- (length ellipsis) (if (eq doom-print-backend 'ansi) 2 4))))
      (dolist (group (cons toplevel rest))
        (let ((label (if (car-safe group) (cdr commands))))
          (when label
            (insert! ((bold "%s:") (car group)) "\n"))
          (print-group! :if label
            (dolist (command (cdr group))
              (let* ((cli  (doom-cli-get command t))
                     (rcli (doom-cli-get command))
                     (summary (doom-cli-short-docs rcli))
                     (subcommands? (doom-cli-subcommands cli 1 :predicate? t)))
                (insert! ((format "%%-%ds%%s%%s"
                                  (+ (- minwidth doom-print-indent)
                                     doom-print-indent-increment
                                     (if subcommands? ellipsislen 0)))
                          (concat (doom-cli-command-string (seq-drop command drop))
                                  (if subcommands? ellipsis))
                          (if inline? " " "\n")
                          (indent (if (and (doom-cli-alias cli)
                                           (not (doom-cli-type rcli)))
                                      (dark "-> %s" (doom-cli-command-string cli))
                                    (when docs?
                                      (if summary (markup summary) (dark "TODO"))))))
                         "\n")))
            (when (cdr rest)
              (insert "\n")))))
      (string-trim-right (buffer-string)))))

;;; Help: options
(defun doom-cli-help--options (cli &optional noformatting?)
  "Return an alist summarizing CLI's options.

The alist's CAR are lists of formatted switches plus their arguments, e.g.
'((\"`--foo'\" \"`BAR'\") ...). Their CDR is their formatted documentation."
  (let* ((docs (doom-cli-help--parse-docs (doom-cli-find cli t) "OPTIONS"))
         (docs (mapcar (fn! (cons (split-string (car %) ", ")
                                  (cdr %)))
                       docs))
         (strfmt (if noformatting? "%s" "`%s'"))
         local-options
         global-options
         seen)
    (dolist (neighbor (nreverse (doom-cli-find cli)))
      (dolist (option (doom-cli-options neighbor))
        (when-let* ((switches (cl-loop for sw in (doom-cli-option-switches option)
                                       if (and (doom-cli-option-flag-p option)
                                               (string-prefix-p "--" sw))
                                       collect (format "--[no-]%s" (substring sw 2))
                                       else collect sw))
                    (switches (seq-difference switches seen)))
          (dolist (switch switches) (push switch seen))
          (push (cons (cl-loop for switch in switches
                               if (doom-cli-option-arguments option)
                               collect (cons (format strfmt switch)
                                             (append (doom-cli-help--parse-args it noformatting?)
                                                     (when (doom-cli-option-multiple-p option)
                                                       (list "..."))))
                               else collect (list (format strfmt switch)))
                      (string-join
                       (or (delq
                            nil (cons (when-let (docs (doom-cli-option-docs option))
                                        (concat docs "."))
                                      (cl-loop for (flags . docs) in docs
                                               unless (equal (seq-difference flags switches) flags)
                                               collect docs)))
                           '("TODO"))
                       "\n\n"))
                (if (equal (doom-cli-command neighbor)
                           (doom-cli-command cli))
                    local-options
                  global-options)))))
    `((local  . ,(nreverse local-options))
      (global . ,(nreverse global-options)))))

(defun doom-cli-help--render-options (options &optional cli)
  (let ((doom-print-indent 0)
        (local  (assq 'local options))
        (global (assq 'global options)))
    (when (or (cdr local) (cdr global))
      (letf! (defun printopts (opts)
               (pcase-dolist (`(,switches . ,docs) (cdr opts))
                 (let (multiple?)
                   (insert!
                    ("%s%s\n%s"
                     (mapconcat
                      (fn! (when (member "..." (cdr %))
                             (setq multiple? t))
                           (string-trim-right
                            (format "%s %s"
                                    (doom-print--cli-markup (car %))
                                    (doom-print--cli-markup
                                     (string-join (remove "..." (cdr %)) "|")))))
                      switches
                      ", ")
                     (if multiple? ", ..." "")
                     (indent (fill (markup docs)) doom-print-indent-increment))
                            "\n\n"))))
        (with-temp-buffer
          (if (null (cdr local))
              (insert (if global "This command has no local options.\n" "") "\n")
            (printopts local))
          (when (cdr global)
            (insert! ((bold "Global options:\n")))
            (print-group! (printopts global)))
          (string-trim-right (buffer-string)))))))

;;; Help: internal
(defun doom-cli-help--parse-args (args &optional noformatting?)
  (cl-loop for arg in args
           if (listp arg)
           collect (string-join (doom-cli-help--parse-args arg noformatting?) "|")
           else if (symbolp arg)
           collect (format (if noformatting? "%s" "`%s'") (upcase (symbol-name arg)))
           else collect arg))

(defun doom-cli-help--parse-docs (cli-list section-name)
  (cl-check-type section-name string)
  (let (alist)
    (dolist (cli cli-list (nreverse alist))
      (when-let (section (cdr (assoc section-name (doom-cli-docs cli))))
        (with-temp-buffer
          (save-excursion (insert section))
          (let ((lead (current-indentation))
                (buffer (current-buffer)))
            (while (not (eobp))
              (let ((heading (string-trim (buffer-substring (point-at-bol) (point-at-eol))))
                    (beg (point-at-bol 2))
                    end)
                (forward-line 1)
                (while (and (not (eobp))
                            (/= (current-indentation) lead)
                            (forward-line 1)))
                (setf (alist-get heading alist nil nil #'equal)
                      (string-join
                       (delq
                        nil (list (alist-get heading alist nil nil #'equal)
                                  (let ((end (point)))
                                    (with-temp-buffer
                                      (insert-buffer-substring buffer beg end)
                                      (goto-char (point-min))
                                      (indent-rigidly (point-min) (point-max) (- (current-indentation)))
                                      (string-trim-right (buffer-string))))))
                       "\n\n"))))))))))

(provide 'doom-cli-help)
;;; help.el ends here
