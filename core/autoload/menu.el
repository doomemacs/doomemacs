;;; ../core/autoload/menu.el -*- lexical-binding: t; -*-

;; Command dispatchers: basically M-x, but context sensitive, customizable and
;; persistent across Emacs sessions.

(defvar doom-menu-display-fn #'doom-menu-read-default
  "The method to use to prompt the user with the menu. This takes two arguments:
PROMPT (a string) and COMMAND (a list of command plists; see `def-menu!').")

(defvar-local doom-menu-last-command nil
  "TODO")

(defun doom-menu-read-default (prompt commands)
  "Default method for displaying a completion-select prompt."
  (completing-read prompt (mapcar #'car commands) nil nil nil nil (car doom-menu-last-command)))

;;;###autoload
(defun doom--menu-read (prompt commands)
  (if-let* ((choice (funcall doom-menu-display-fn prompt commands)))
      (assoc choice commands)
    (user-error "Aborted")))

;;;###autoload
(defun doom--menu-exec (plist)
  (save-selected-window
    (let ((command (plist-get plist :exec))
          (cwd     (plist-get plist :cwd)))
      (let ((default-directory
              (cond ((eq cwd t) (doom-project-root))
                    ((stringp cwd) cwd)
                    ((functionp cwd) (funcall cwd))
                    (t default-directory))))
        (cond ((stringp command)
               (let (buf)
                 (compile command)
                 (setq buf next-error-last-buffer)
                 (unless buf
                   (error "Couldn't create compilation buffer"))
                 (with-current-buffer buf
                   (setq header-line-format
                         (concat (propertize "$ " 'face 'font-lock-doc-face)
                                 (propertize command 'face 'font-lock-preprocessor-face))))))
              ((or (symbolp command)
                   (functionp command))
               (call-interactively command))
              ((and command (listp command))
               (eval command t))
              (t
               (error "Not a valid command: %s" command)))))))

;;;###autoload
(defmacro def-menu! (name desc commands &rest plist)
  "Defines a menu and returns a function symbol for invoking it.

A dispatcher is an interactive command named NAME (a symbol). When called, this
dispatcher prompts you to select a command to run. This list is filtered
depending on its properties. Each command is takes the form of:

  (DESCRIPTION :exec COMMAND &rest PROPERTIES)

PROPERTIES accepts the following properties:

  :when FORM
  :unless FORM
  :region BOOL
  :cwd BOOL|PATH|FUNCTION
  :project BOOL|PATH|FUNCTION

COMMAND can be a string (a shell command), a symbol (an elisp function) or a
lisp form.

`def-menu!'s PLIST supports the following properties:

  :prompt STRING"
  (declare (indent defun) (doc-string 2))
  (let ((commands-var (intern (format "%s-commands" name)))
        (prop-prompt (or (plist-get plist :prompt) "> "))
        (prop-sort   (plist-get plist :sort)))
    `(progn
       (defconst ,commands-var
         ,(if prop-sort
              `(cl-sort ,commands #'string-lessp :key #'car)
            commands)
         ,(format "Menu for %s" name))
       (defun ,name (arg command)
         ,(concat
           (if (stringp desc) (concat desc "\n\n"))
           "This is a command dispatcher. It will rerun the last command on\n"
           "consecutive executions. If ARG (universal argument) is non-nil\n"
           "then it always prompt you.")
         (declare (interactive-only t))
         (interactive
          (list current-prefix-arg
                (progn
                  (unless ,commands-var
                    (user-error "The '%s' menu is empty" ',name))
                  (doom--menu-read
                   ,prop-prompt
                   (or (cl-remove-if-not
                        (let ((project-root (doom-project-root)))
                          (lambda (cmd)
                            (let ((plist (cdr cmd)))
                              (and (cond ((not (plist-member plist :region)) t)
                                         ((plist-get plist :region) (use-region-p))
                                         (t (not (use-region-p))))
                                   (let ((when (plist-get plist :when))
                                         (unless (plist-get plist :unless))
                                         (project (plist-get plist :project)))
                                     (when (functionp project)
                                       (setq project (funcall project)))
                                     (or (or (not when) (eval when))
                                         (or (not unless) (not (eval unless)))
                                         (and (stringp project)
                                              (file-in-directory-p (or buffer-file-name default-directory)
                                                                   project-root))))))))
                        ,commands-var)
                       (user-error "No commands available here"))))))
         (doom--menu-exec
          (cdr (or (when arg doom-menu-last-command)
                   (setq doom-menu-last-command command)
                   (user-error "No command selected"))))))))

