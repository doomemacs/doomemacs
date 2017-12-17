;;; ../core/autoload/menu.el -*- lexical-binding: t; -*-

;; Command dispatchers: basically M-x, but context sensitive, customizable and
;; persistent across Emacs sessions.

(defvar doom-menu-display-fn #'doom-menu-read-default
  "The method to use to prompt the user with the menu. This takes two arguments:
PROMPT (a string) and COMMAND (a list of command plists; see `def-menu!').")

(defun doom-menu-read-default (prompt commands)
  "Default method for displaying a completion-select prompt."
  (completing-read prompt (mapcar #'car commands)))

(defun doom--menu-read (prompt commands)
  (if-let* ((choice (funcall doom-menu-display-fn prompt commands)))
      (cdr (assoc choice commands))
    (user-error "Aborted")))

(defun doom--menu-exec (plist)
  (let ((command (plist-get plist :exec))
        (cwd     (plist-get plist :cwd)))
    (let ((default-directory
            (cond ((eq cwd t) (doom-project-root))
                  ((stringp cwd) cwd)
                  (t default-directory))))
      (cond ((stringp command)
             (with-current-buffer (get-buffer-create "*compilation*")
               (setq command (doom-resolve-vim-path command))
               (save-window-excursion
                 (compile command))
               (setq header-line-format
                     (concat (propertize "$ " 'face 'font-lock-doc-face)
                             (propertize command 'face 'font-lock-preprocessor-face)))
               (doom-resize-window
                (doom-popup-buffer (current-buffer)
                  '(:autokill t :autoclose t)) 12)))
            ((or (symbolp command)
                 (functionp command))
             (call-interactively command))
            ((and command (listp command))
             (eval command t))
            (t
             (error "Not a valid command: %s" command))))))

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
  :cwd t|PATH
  :project BOOL|DIRECTORY

COMMAND can be a string (a shell command), a symbol (an elisp function) or a
lisp form.

`def-menu!'s PLIST supports the following properties:

  :prompt STRING"
  (declare (indent defun) (doc-string 2))
  (let ((commands-var (intern (format "%s-commands" name)))
        (prop-prompt (or (plist-get plist :prompt) "> "))
        (prop-sort   (plist-get plist :sort)))
    `(progn
       (defvar ,commands-var
         ,(if prop-sort
              `(cl-sort ,commands #'string-lessp :key #'car)
            commands)
         ,(format "Menu for %s" name))
       (defun ,name ()
         ,desc
         (interactive)
         (unless ,commands-var
           (user-error "The '%s' menu is empty" ',name))
         (doom--menu-exec
          (or (doom--menu-read
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
                                 (or (or (not when) (eval when))
                                     (or (not unless) (not (eval unless)))
                                     (and (stringp project)
                                          (file-in-directory-p buffer-file-name project-root))))))))
                    ,commands-var)
                   (user-error "No commands available here")))
              (user-error "No command selected")))))))

