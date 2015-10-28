(eval-when-compile
  (require 'cl-lib)
  (require 'dash))

;; Backwards compatible `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro λ (&rest body)
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
       (,@(when match
            `(add-to-list ',(if minor-p 'narf-auto-minor-mode-alist 'auto-mode-alist)
                          (cons ,match ',mode))))
       (,@(when files
            (unless (or (listp files) (stringp files))
              (user-error "associate! :files expects a string or list of strings"))
            (let ((hook-name (intern (format "narf--init-mode-%s" mode))))
              `(progn
                 (defun ,hook-name ()
                   (when (and (not ,mode)
                              (narf/project-has-files ,@(-list files)))
                     (,mode 1)))
                 ,@(if (and in (listp in))
                       (mapcar (lambda (h) `(add-hook ',h ',hook-name))
                               (mapcar (lambda (m) (intern (format "%s-hook" m))) in))
                     `((add-hook 'find-file-hook ',hook-name))))))))))

(after! evil
  ;; Register keywords for proper indentation (see `bind!')
  (put ':prefix  'lisp-indent-function 'defun)
  (put ':map     'lisp-indent-function 'defun)
  (put ':after   'lisp-indent-function 'defun)
  (put ':when    'lisp-indent-function 'defun)
  (put ':unless  'lisp-indent-function 'defun)

  (defmacro bind! (&rest rest)
    (let ((i 0)
          key def
          first-set
          prefix
          internal
          (default-keymaps '((current-global-map)))
          (keymaps (if (boundp 'keymaps) keymaps))
          (states  (if (boundp 'states) states '()))
          (forms '())
          (state-map '(("n" . normal)
                       ("v" . visual)
                       ("i" . insert)
                       ("e" . emacs)
                       ("o" . operator)
                       ("m" . motion)
                       ("r" . replace))))
      (unless keymaps
        (setq keymaps default-keymaps))
      (while rest
        (setq key (pop rest))
        (add-to-list
         'forms
         (cond ((eq key '-) nil) ; skip this

               ((listp key) ; it's a sub exp
                `((bind! ,@key)))

               ((keywordp key)
                (pcase key
                  ;; TODO: Data checks
                  (:prefix      (setq prefix (kbd (pop rest)))
                    (if (= i 0) (setq first-set `(:prefix . ,prefix)))
                    nil)
                  (:map         (setq keymaps (-list (pop rest)))
                    (if (= i 0) (setq first-set `(:map . ,keymaps)))
                    nil)
                  (:unset       `((bind! ,(kbd (pop rest)) nil)))
                  (:after       (prog1 `((after! ,(pop rest)   (bind! ,@rest))) (setq rest '())))
                  (:when        (prog1 `((if ,(pop rest)       (bind! ,@rest))) (setq rest '())))
                  (:unless      (prog1 `((if (not ,(pop rest)) (bind! ,@rest))) (setq rest '())))
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
                (let ((first-key (car first-set))
                      (first-value (cdr first-set))
                      out-forms)
                  (dolist (keymap keymaps)
                    (if (not states)
                        (add-to-list 'out-forms `(evil-define-key nil ,keymap ,key ,def) t)
                      (dolist (state states)
                        (add-to-list 'out-forms `(evil-define-key ',state ,keymap ,key ,def) t))))
                  (setq prefix  (if (eq first-key :prefix) first-value))
                  (setq keymaps (if (eq first-key :map) first-value default-keymaps))
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

(defun narf|enable-tab-width-2 ()
  (setq tab-width 2 evil-shift-width 2))

(defun narf|enable-tab-width-4 ()
  (setq tab-width 4 evil-shift-width 4))

(defun narf|disable-final-newline ()
  (set (make-local-variable 'require-final-newline) nil))

(defun narf|enable-tabs ()
  (setq indent-tabs-mode t))

(defun narf|disable-tabs ()
  (setq indent-tabs-mode nil))

(defun narf|disable-delete-trailing-whitespace ()
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))

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
      (when (get-buffer "*Completions*")
        (delete-windows-on "*Completions*"))
      (abort-recursive-edit))))

(after! evil
  (evil-define-command narf:exit-mode-maybe ()
    "Exits insert/replace mode using jk without the momentary pause caused by
key-chord-define."
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (call-interactively 'self-insert-command)
    (let ((evt (read-event nil nil 0.4)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (or (char-equal evt ?k)
                                (char-equal evt ?K)))
        (if (evil-replace-state-p)
            (evil-replace-backspace)
          (delete-char -1))
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t
        (setq unread-command-events (append unread-command-events (list evt)))))))))

(provide 'core-defuns)
;;; core-defuns.el ends here
