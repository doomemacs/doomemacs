(eval-when-compile (require 'cl-lib))

;; Backwards compatible `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro λ! (&rest body)
  "A shortcut for: `(lambda () (interactive) ,@body)"
  `(lambda () (interactive) ,@body))

(defmacro shut-up! (&rest body)
  "Silence message output from code."
  (declare (indent defun))
  `(let (message-log-max) ,@body (message "")))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load', that supresses warnings
during compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro in! (dir &rest forms)
  (declare (indent defun))
  `(let ((default-directory ,dir))
     ,@forms))

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be one quoted symbol, a list of quoted symbols, or a series of
forms. Forms will be wrapped in one lambda. A list of symbols will expand into a
series of add-hook calls.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    => (add-hook 'some-mode-hook 'enable-something)

    (add-hook! some-mode '(enable-something and-another))
    => (add-hook 'some-mode-hook 'enable-something)
       (add-hook 'some-mode-hook 'and-another)

    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    => (add-hook 'one-mode-hook 'enable-something)
       (add-hook 'second-mode-hook 'enable-something)

    (add-hook! (one-mode second-mode) 'enable-something)
    => (add-hook 'one-mode-hook 'enable-something)
       (add-hook 'second-mode-hook 'enable-something)

    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    => (add-hook 'one-mode-hook (lambda () (setq v 5) (setq a 2)))
       (add-hook 'second-mode-hook (lambda () (setq v 5) (setq a 2)))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted (eq (car-safe hook) 'quote))
         (hook (if quoted (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms)))
         (forms '()))
    (mapc
     (lambda (f) (let ((func (cond ((symbolp f) `(quote ,f))
                                   (t `(lambda () ,@func-or-forms)))))
                   (mapc
                    (lambda (h) (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
                    (if (listp hook) hook (list hook))))) funcs)
    `(progn ,@forms)))

(cl-defmacro associate! (mode &key in
                              &key match
                              &key files
                              &allow-other-keys)
  "Associate a major or minor mode to certain patterns and project files."
  (declare (indent 1))
  (let* ((minor-p (assoc mode minor-mode-alist)))
    `(progn
       (,@(cond ((or files in)
                 (when (and files (not (or (listp files)
                                           (stringp files))))
                   (user-error "associate! :files expects a string or list of strings"))
                 (let ((hook-name (intern (format "narf--init-mode-%s" mode))))
                   `(progn
                      (defun ,hook-name ()
                        (when (and (if ,match (string-match-p ,match (buffer-file-name)) t)
                                   (or ,(not files)
                                       (and (boundp ',mode)
                                            (not ,mode)
                                            (narf/project-has-files ,@(-list files)))))
                          (,mode 1)))
                      ,@(if (and in (listp in))
                            (mapcar (lambda (h) `(add-hook ',h ',hook-name))
                                    (mapcar (lambda (m) (intern (format "%s-hook" m))) in))
                          `((add-hook 'find-file-hook ',hook-name))))))
                (match
                 `(add-to-list ',(if minor-p 'narf-auto-minor-mode-alist 'auto-mode-alist)
                               (cons ,match ',mode)))
                (t (user-error "associate! invalid rules for mode [%s] (in %s) (match %s) (files %s)"
                               mode in match files)))))))

(after! evil
  ;; Register keywords for proper indentation (see `map!')
  (put ':prefix      'lisp-indent-function 'defun)
  (put ':map         'lisp-indent-function 'defun)
  (put ':after       'lisp-indent-function 'defun)
  (put ':when        'lisp-indent-function 'defun)
  (put ':unless      'lisp-indent-function 'defun)
  (put ':leader      'lisp-indent-function 'defun)
  (put ':localleader 'lisp-indent-function 'defun)

  (defmacro map! (&rest rest)
    (let ((i 0)
          (forms '())
          (keymaps (if (boundp 'keymaps) keymaps))
          (default-keymaps '((current-global-map)))
          (state-map '(("n" . normal)
                       ("v" . visual)
                       ("i" . insert)
                       ("e" . emacs)
                       ("o" . operator)
                       ("m" . motion)
                       ("r" . replace)))
          (prefix (if (boundp 'prefix) prefix))
          key def states)
      (unless keymaps
        (setq keymaps default-keymaps))
      (while rest
        (setq key (pop rest))
        (add-to-list
         'forms
         (cond ((listp key) ; it's a sub exp
                `(,(macroexpand `(map! ,@key))))

               ((keywordp key)
                (when (memq key '(:leader :localleader))
                  (push (pcase key
                          (:leader narf-leader-prefix)
                          (:localleader narf-localleader-prefix))
                        rest)
                  (setq key :prefix))
                (pcase key
                  ;; TODO: Data checks
                  (:prefix      (setq prefix (concat prefix (kbd (pop rest)))) nil)
                  (:map         (setq keymaps (-list (pop rest))) nil)
                  (:unset      `(,(macroexpand `(map! ,(kbd (pop rest)) nil))))
                  (:after       (prog1 `((after! ,(pop rest)   ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                  (:when        (prog1 `((if ,(pop rest)       ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                  (:unless      (prog1 `((if (not ,(pop rest)) ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                  (otherwise ; might be a state prefix
                   (mapc (lambda (letter)
                           (if (assoc letter state-map)
                               (add-to-list 'states (cdr (assoc letter state-map)))
                             (user-error "Invalid mode prefix %s in key %s" letter key)))
                         (split-string (substring (symbol-name key) 1) "" t))
                   (unless states
                     (user-error "Unrecognized keyword %s" key)) nil)))

               ;; It's a key-def pair
               ((or (stringp key)
                    (characterp key)
                    (vectorp key))
                (when (stringp key)
                  (setq key (kbd key)))
                (when prefix
                  (setq key (cond ((vectorp key) (vconcat prefix key))
                                  (t (concat prefix key)))))
                (unless (> (length rest) 0)
                  (user-error "Map has no definition for %s" key))
                (setq def (pop rest))
                (let (out-forms)
                  (dolist (keymap keymaps)
                    (if (not states)
                        (add-to-list 'out-forms `(evil-define-key nil ,keymap ,key ,def) t)
                      (dolist (state states)
                        (add-to-list 'out-forms `(evil-define-key ',state ,keymap ,key ,def) t))))
                  (setq states '())
                  out-forms))

               (t (user-error "Invalid key %s" key)))
         t)
        (cl-incf i))
      `(progn ,@(apply #'nconc (delete nil (delete (list nil) forms)))))))


;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun narf|enable-comment-hard-wrap ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill))

(defun narf|enable-hard-wrap ()
  (turn-on-auto-fill))

(defun narf|update-scratch-buffer-cwd () ; see core-editor.el
  "Make sure scratch buffer is always 'in a project.'"
  (let ((dir (narf/project-root)))
      (with-current-buffer (get-buffer-create "*scratch*")
        (cd dir))))


;;;; Global Defuns ;;;;;;;;;;;;;;;;;;;;;
(defun narf-minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (let (message-log-max)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark t)
      (abort-recursive-edit))))

(defun narf-reload ()
  "Reload `load-path', in case you updated cask while emacs was open!"
  (interactive)
  (setq load-path (append (list narf-private-dir narf-core-dir narf-modules-dir narf-packages-dir)
                          (f-directories narf-core-dir nil t)
                          (f-directories narf-modules-dir nil t)
                          (f-directories narf-packages-dir)
                          (f-directories (expand-file-name "../bootstrap" narf-packages-dir))
                          narf--load-path)))

(defun narf-reload-autoloads ()
  "Regenerate autoloads for NARF emacs."
  (interactive)
  (let ((generated-autoload-file (concat narf-core-dir "/autoloads.el")))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (mapc (lambda (dir)
            (update-directory-autoloads (concat dir "/defuns"))
            (message "Scanned: %s" dir))
          (list narf-core-dir narf-modules-dir))
    (when (called-interactively-p 'interactive)
      (require 'autoloads))
    (message "Done!")))

(provide 'core-defuns)
;;; core-defuns.el ends here
