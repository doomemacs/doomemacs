(! (defalias '@--concat-forms 'use-package-concat)
   (defalias '@--normalize-symbols 'use-package-normalize-symlist)
   (defalias '@--normalize-paths 'use-package-normalize-paths)


   ;; Backwards compatible `with-eval-after-load'
   (unless (fboundp 'with-eval-after-load)
     (defmacro with-eval-after-load (file &rest body)
       `(eval-after-load ,file
          `(funcall (function ,(lambda () ,@body))))))

   (defmacro @after (feature &rest forms)
     (declare (indent 1))
     `(,(if (or (not (boundp 'byte-compile-current-file))
                (not byte-compile-current-file)
                (if (symbolp feature)
                    (require feature nil :no-error)
                  (load feature :no-message :no-error)))
            'progn
          (message "after: cannot find %s" feature)
          'with-no-warnings)
       (with-eval-after-load ',feature ,@forms)))

   (defmacro @shut-up (&rest body)
     "Silence message output from code."
     (declare (indent defun))
     `(let (message-log-max) ,@body (message "")))

   (defmacro @ (args &rest body)
       "A shortcut for: `(lambda ,args ,@body)"
     `(lambda ,args ,@body))

   (defmacro λ (&rest body)
     "A shortcut for: `(lambda () (interactive) ,@body)"
     `(lambda () (interactive) ,@body))

   (defmacro @add-hook (hook &rest func-or-forms)
     "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be one quoted symbol, a list of quoted symbols, or a series of
forms. Forms will be wrapped in one lambda. A list of symbols will expand into a
series of add-hook calls.

Examples:
    (@add-hook 'some-mode-hook 'enable-something)
    => (add-hook 'some-mode-hook 'enable-something)

    (@add-hook some-mode '(enable-something and-another))
    => (add-hook 'some-mode-hook 'enable-something)
       (add-hook 'some-mode-hook 'and-another)

    (@add-hook '(one-mode-hook second-mode-hook) 'enable-something)
    => (add-hook 'one-mode-hook 'enable-something)
       (add-hook 'second-mode-hook 'enable-something)

    (@add-hook (one-mode second-mode) 'enable-something)
    => (add-hook 'one-mode-hook 'enable-something)
       (add-hook 'second-mode-hook 'enable-something)

    (@add-hook (one-mode second-mode) (setq v 5) (setq a 2))
    => (add-hook 'one-mode-hook (lambda () (setq v 5) (setq a 2)))
       (add-hook 'second-mode-hook (lambda () (setq v 5) (setq a 2)))"
     (declare (indent 1))
     (unless func-or-forms
       (error "@add-hook: FUNC-OR-FORMS is empty"))
     (let* ((val (car func-or-forms))
            (quoted (eq (car-safe hook) 'quote))
            (hook (if quoted (cadr hook) hook))
            (funcs (if (eq (car-safe val) 'quote)
                       (if (cdr-safe (cadr val))
                           (cadr val)
                         (list (cadr val)))
                     (list func-or-forms)))
            (forms '()))
       (mapc (@ (f)
                (let ((func (cond ((symbolp f) `(quote ,f))
                                  (t `(lambda () ,@func-or-forms)))))
                  (mapc (@ (h)
                           (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
                        (if (listp hook) hook (list hook))))) funcs)
       `(progn ,@forms)))

   (cl-defmacro @associate (mode &key in
                                 &key match
                                 &key files
                                 &allow-other-keys)
     "Associate a major or minor mode to certain patterns and project files."
     (let* ((minor-p (memq mode minor-mode-alist))
            (modes (@--normalize-symbols ":in" in)))
       (@--concat-forms
        (when match
          `(add-to-list ,(if minor-p 'narf-auto-minor-mode-alist 'auto-mode-alist)
                        (cons ,match ,mode)))
        (when files
          `(defun ,(intern (format "narf|init-mode-%s" 'lb6-mode)) ()
             (when (and (assq major-mode '(,@(@--normalize-paths ":in" in)))
                        (narf-project-has-files ,@(@--normalize-paths ":files" files)))
               (,mode 1)))))))

   (@after evil
     ;; Placeholders to correct binding indentation. Don't use these.
     (defmacro :leader (key &rest rest) (declare (indent 1)))
     (defmacro :localleader (key &rest rest) (declare (indent 1)))
     (defmacro :map (key &rest rest) (declare (indent 1)))
     (defmacro :after (key &rest rest) (declare (indent 1)))
     (defmacro :when (key &rest rest) (declare (indent 1)))

     (macroexpand `(@map (:map my-map "C-k" 'hello :n "C-p" 'goodbye)))

     (defmacro @map (&rest rest)
       (declare (indent defun))
       (let ((i 0)
             key def
             first-set
             prefix
             (default-keymaps '(narf-mode-map))
             (keymaps (if (boundp 'keymaps) keymaps))
             (states  (if (boundp 'states) states '()))
             (forms (if (boundp 'forms) forms))
             (state-map '(("n" . normal)
                          ("v" . visual)
                          ("i" . insert)
                          ("e" . emacs)
                          ("o" . operator)
                          ("m" . motion)
                          ("r" . replace)
                          ("I" . iedit))))
         (unless keymaps
           (setq keymaps default-keymaps))
         (while rest
           (setq key (pop rest))
           (message ">>> KEY: %s" key)
           (add-to-list
            'forms
            (cond ((eq key '-)) ; skip this

                  ((listp key) ; it's a sub exp
                   (macroexpand `(@map ,@key)))

                  ((keywordp key)
                   (pcase key
                     ;; TODO: Data checks
                     (:leader      (setq prefix (kbd (pop rest))) nil)
                     (:localleader (setq prefix (kbd (pop rest))) nil)
                     (:prefix      (setq prefix (kbd (pop rest)))
                                   (if (= i 0) (setq first-set `(:prefix . ,prefix)))
                                   nil)
                     (:map         (setq keymaps (-list (pop rest)))
                                   (if (= i 0) (setq first-set `(:map . ,keymaps)))
                                   nil)
                     (:unset       (prog1 `(@map ,(kbd (pop rest)) nil)))
                     (:after       (prog1 `(@after ,(pop rest) ,(macroexp-progn `(@map ,@rest))) (setq rest '())))
                     (:when        (prog1 `(when ,(pop rest) ,(macroexp-progn `(@map ,@rest))) (setq rest '())))
                     (:unless      (prog1 `(unless ,(pop rest) ,(macroexp-progn `(@map ,@rest))) (setq rest '())))
                     (otherwise ; might be a state prefix
                      (mapc (lambda (letter)
                              (when (assoc letter state-map)
                                (add-to-list 'states (cdr (assoc letter state-map)))))
                            (s-split "" (substring (symbol-name key) 1) t)) nil)))

                  ;; It's a key-def pair
                  ((or (stringp key)
                       (characterp key)
                       (vectorp key))

                   (when (stringp key)
                     (setq key (kbd key)))
                   (when prefix
                     (cond ((vectorp key)
                            (setq key (vconcat prefix key)))
                           (t
                            (setq key (concat prefix key)))))

                   (unless (car rest)
                     (user-error "Map has no definition for %s" key))

                   (setq def (pop rest))
                   (let ((first-key (car first-set))
                         (first-value (cdr first-set))
                         out-forms)
                     (dolist (keymap keymaps)
                       (if (not states)
                           (add-to-list 'out-forms `(define-key ,keymap ,key ,def) t)
                         (dolist (state states)
                           (add-to-list 'out-forms `(define-key (evil-get-auxiliary-keymap ,keymap ,state t) ,key ,def) t))))

                     (setq prefix  (if (eq first-key :prefix) first-value))
                     (setq keymaps (if (eq first-key :map) first-value default-keymaps))
                     (setq states '())
                     out-forms))

                  (t (user-error "" key)))
            t)
           (cl-incf i))
         `(progn ,@(apply #'nconc (delete nil (delete (list nil) forms))))))

     ;;  (defmacro @map (&rest keys)
     ;;    "A minimalistic and evil-centric way of binding keys. KEYS is
     ;;made up of either:
     ;;
     ;;1. Any of the following keywords:
     ;;
     ;;:when CONDITION
     ;;:unless CONDITION
     ;;:prefix PREFIX          Key(s) to prefix keymappings with
     ;;:map KEYMAP             Keymaps to bind keys to. Can be a list.
     ;;:global                 Tags these keymaps for the global keymap
     ;;:local                  Ditto, but for local keymap
     ;;
     ;;
     ;;2. A key (as a vector e.g. [escape], a string \"<escape>\", or
     ;;character ?\^?).
     ;;
     ;;3. A key definition: a symbol or a lambda function. "
     ;;    (declare (indent defun))
     ;;    (let* ((keymaps (-list map))
     ;;           (states (-list in))
     ;;           (forms '())
     ;;           item def)
     ;;      (while keys
     ;;        (setq item (pop keys))
     ;;        (cond ((keywordp item)
     ;;               (let ((val (pop keys)))
     ;;                 (pcase item
     ;;                   (:after)
     ;;                   (:when)
     ;;                   (:unless)
     ;;                   (:keymap)
     ;;                   (:in)
     ;;                   (otherwise)
     ;;                   )
     ;;                 ))
     ;;
     ;;              ((or (and (symbolp item)
     ;;                        (evil-state-p item))
     ;;                   (and (listp item)
     ;;                        (--all? (evil-state-p it) item)))
     ;;               (setq states (-list item)))
     ;;
     ;;              ;; item-definition pairs
     ;;              ((consp item)
     ;;               (let ((def (cdr item))
     ;;                     (item (car item)))
     ;;                 (message "k %s : d %s" item def)
     ;;
     ;;                 ;;(or (stringp item)
     ;;                 ;;    (vectorp item)
     ;;                 ;;    (characterp item))
     ;;                 ;;(unless items (signal 'bind-no-definition item))
     ;;                 ;;(setq def (pop items))
     ;;                 (when condition
     ;;                   ;; Process the item
     ;;                   (cond ((stringp item)    (setq item (kbd item)))
     ;;                         ((characterp item) (setq item (string item))))
     ;;                   (when prefix
     ;;                     (setq item (if (vectorp item)
     ;;                                    (vconcat prefix item)
     ;;                                  (concat (kbd prefix) item))))
     ;;                   ;; Do the binding
     ;;                   `(,@(if (null states)
     ;;                       (push (mapcar
     ;;                              (lambda (keymap) `(define-key ,keymap ,item ,def)) keymaps)
     ;;                             forms)
     ;;                     (push (mapcar (lambda (state)
     ;;                               (mapcar (lambda (keymap)
     ;;                                         `(define-key (evil-get-auxiliary-keymap ,keymap ',state t) ,item ,def))
     ;;                                       keymaps))
     ;;                             states) forms))))))
     ;;
     ;;              ;; fallback
     ;;              (t (signal 'bind-invalid-key key)))
     ;;        `(progn ,@forms))))

     (defmacro @exmap (command func)
       (evil-ex-define-cmd
        ,command
        ,(cond ((autoloadp (symbol-function `,func))
                `(lambda () (interactive) (call-interactively ,func)))
               ((symbolp `,func) func)
               (t (user-error "Command for %s is invalid" command)))))))


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
  (let ((dir (narf-project-root)))
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


;;;; Project defuns ;;;;;;;;;;;;;;;;;;;;
(defun narf-project-root (&optional strict-p)
  "Get the path to the root of your project. Uses `narf-project-root-files' to
determine if a directory is a project."
  (let ((home (file-truename "~")))
    (catch 'found
      (f-traverse-upwards
       (lambda (path)
         (let ((path (file-truename path)))
           (if (file-equal-p home path)
               (throw 'found (if strict-p nil default-directory))
             (dolist (file narf-project-root-files)
               (when (file-exists-p (expand-file-name file path))
                 (throw 'found path)))))) default-directory)
    default-directory)))

(defun narf-project-has-files (files &optional root)
  "Return non-nil if `file' exists in the project root."
  (let ((root (or root (narf-project-root)))
        (files (if (listp files) files (list files)))
        found-p file)
    (while (and files (not found-p))
      (setq file (pop files))
      (setq found-p (file-exists-p (narf-project-path-to file root))))
    found-p))

(defun narf-project-path-to (file &optional root)
  (let ((root (or root (narf-project-root))))
    (expand-file-name file root)))

(defun narf-project-name (&optional root)
  (file-name-nondirectory (directory-file-name (or root (narf-project-root)))))

(defun narf-project-p ()
  (not (null (narf-project-root t))))


(provide 'core-defuns)
;;; core-defuns.el ends here
