;;; core-defuns.el

;; Bootstrap macro
(defmacro doom (&rest packages)
  "Bootstrap DOOM emacs and initialize PACKAGES"
  `(let (file-name-handler-alist)
     ;; Local settings
     (load "~/.emacs.local.el" t t)
     ;; Bootstrap
     (unless noninteractive
       ,@(mapcar (lambda (pkg)
                   (let ((lib-path (locate-library (symbol-name pkg))))
                     (unless lib-path
                       (error "Initfile not found: %s" pkg))
                     `(require ',pkg ,(file-name-sans-extension lib-path))))
                 packages)
       (when window-system
         (require 'server)
         (unless (server-running-p)
           (server-start)))
       ;; Prevent any auto-displayed text + benchmarking
       (advice-add 'display-startup-echo-area-message :override 'ignore)
       (message ""))
     (setq-default gc-cons-threshold 4388608
                   gc-cons-percentage 0.4)))

;; Backwards compatible `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro λ! (&rest body)
  "A shortcut for inline keybind lambdas."
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

(defmacro noop! (name &optional args)
  `(defun ,name ,args (interactive) (error "%s not implemented!" name)))

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
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
     (lambda (f)
       (let ((func (cond ((symbolp f) `(quote ,f))
                         (t `(lambda (&rest _) ,@func-or-forms)))))
         (mapc
          (lambda (h)
            (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
          (-list hook)))) funcs)
    `(progn ,@forms)))

(defmacro associate! (mode &rest rest)
  "Associate a major or minor mode to certain patterns and project files."
  (declare (indent 1))
  (let ((minor (plist-get rest :minor))
        (in    (plist-get rest :in))
        (match (plist-get rest :match))
        (files (plist-get rest :files))
        (pred  (plist-get rest :when)))
    `(progn
       (,@(cond ((or files in pred)
                 (when (and files (not (or (listp files) (stringp files))))
                   (user-error "associate! :files expects a string or list of strings"))
                 (let ((hook-name (intern (format "doom--init-mode-%s" mode))))
                   `(progn
                      (defun ,hook-name ()
                        (when (and ,(if match `(if buffer-file-name (string-match-p ,match buffer-file-name)) t)
                                   (or ,(not files)
                                       (and (boundp ',mode)
                                            (not ,mode)
                                            (doom/project-has-files ,@(-list files))))
                                   (or (not ,pred)
                                       (funcall ,pred buffer-file-name)))
                          (,mode 1)))
                      ,@(if (and in (listp in))
                            (mapcar (lambda (h) `(add-hook ',h ',hook-name))
                                    (mapcar (lambda (m) (intern (format "%s-hook" m))) in))
                          `((add-hook 'find-file-hook ',hook-name))))))
                (match
                 `(add-to-list ',(if minor 'doom-auto-minor-mode-alist 'auto-mode-alist)
                               (cons ,match ',mode)))
                (t (user-error "associate! invalid rules for mode [%s] (in %s) (match %s) (files %s)"
                               mode in match files)))))))

(defmacro def-project-type! (name lighter &rest body)
  "Define a minor mode for a specific framework, library or project type."
  (declare (indent 2))
  (let* ((mode-name (format "%s-project-mode" name))
         (mode (intern mode-name))
         (mode-map (intern (format "%s-map" mode-name)))
         (mode-hook-sym (intern (format "%s-hook" mode-name)))
         (mode-init-sym (intern (format "doom--init-project-%s" mode-name))))
    (let ((modes (plist-get body :modes))
          (pred  (plist-get body :when))
          (match (plist-get body :match))
          (files (plist-get body :files))
          (build (plist-get body :build))
          (bind  (plist-get body :bind))
          elem)
      (while (keywordp (car body))
        (pop body)
        (pop body))
      `(progn
         (define-minor-mode ,mode
           "Auto-generated by `def-project-type!'"
           :init-value nil
           :lighter ,(concat " " lighter)
           :keymap (make-sparse-keymap))

         (after! yasnippet
           (add-hook ',mode-hook-sym
                     (lambda ()
                       (if ,mode
                           (yas-activate-extra-mode ',mode)
                         (yas-deactivate-extra-mode ',mode)))))

         ,(when bind `(map! :map ,mode-map ,bind))

         (associate! ,mode
           :minor t
           :in ,modes
           :match ,match
           :files ,files
           :when ,pred)

         (defun ,mode-init-sym ()
           (after! company-dict
             (push ',mode company-dict-minor-mode-list))
           ,(when build
              (let ((cmd build) file)
                (when (and (not (functionp build)) (listp build))
                  (setq cmd (car-safe (cdr-safe build))
                        file (cdr-safe (cdr-safe build))))
                `(def-builder! ,mode ,cmd ,file)))
           ,@body
           (remove-hook ',mode-hook-sym ',mode-init-sym))
         (add-hook ',mode-hook-sym ',mode-init-sym)
         ',mode))))


(after! evil
  (defalias 'ex! 'evil-ex-define-cmd)

  ;; NOTE evil-mode doesn't read local `evil-ex-commands', and will not
  ;; autocomplete local commands.
  (defun ex-local! (cmd fn)
    "Define a buffer-local ex command."
    (unless (local-variable-p 'evil-ex-commands)
      (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
    (evil-ex-define-cmd cmd fn))

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
          key def states forms)
      (unless keymaps
        (setq keymaps default-keymaps))
      (while rest
        (setq key (pop rest))
        (push
         (reverse
          (cond ((listp key) ; it's a sub exp
                 `(,(macroexpand `(map! ,@key))))

                ((keywordp key)
                 (when (memq key '(:leader :localleader))
                   (push (cond ((eq key :leader)
                                doom-leader)
                               ((eq key :localleader)
                                doom-localleader))
                         rest)
                   (setq key :prefix))
                 (pcase key
                   (:prefix  (setq prefix (concat prefix (kbd (pop rest)))) nil)
                   (:map     (setq keymaps (-list (pop rest))) nil)
                   (:unset  `(,(macroexpand `(map! ,(kbd (pop rest)) nil))))
                   (:after   (prog1 `((after! ,(pop rest)   ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                   (:when    (prog1 `((if ,(pop rest)       ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                   (:unless  (prog1 `((if (not ,(pop rest)) ,(macroexpand `(map! ,@rest)))) (setq rest '())))
                   (otherwise ; might be a state prefix
                    (mapc (lambda (letter)
                            (if (assoc letter state-map)
                                (push (cdr (assoc letter state-map)) states)
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
                   (mapc (lambda (keymap)
                           (if states
                               (push `(evil-define-key ',states ,keymap ,key ,def) out-forms)
                             (push `(define-key ,keymap ,key ,def) out-forms)))
                         keymaps)
                   (setq states '())
                   out-forms))
                (t (user-error "Invalid key %s" key))))
         forms)
        (setq i (1+ i)))
      `(progn ,@(apply #'nconc (delete nil (delete (list nil) (reverse forms))))))))

(defmacro def-repeat! (command next-func prev-func)
  "Repeat motions with SPC/S-SPC"
  `(defadvice ,command
       (before ,(intern (format "doom-space--%s" (symbol-name command))) activate)
     (define-key evil-motion-state-map (kbd "SPC") ',next-func)
     (define-key evil-motion-state-map (kbd "S-SPC") ',prev-func)))

;;
(defun doom|update-scratch-buffer (&optional dir inhibit-doom)
  "Make sure scratch buffer is always 'in a project', and looks good."
  (let ((dir (or dir (doom/project-root))))
    (with-current-buffer doom-buffer
      ;; Reset scratch buffer if it wasn't visible
      (when (and (get-buffer-window-list doom-buffer nil t)
                 (not doom-buffer-edited)
                 (not inhibit-doom))
        (doom-mode-init t))
      (setq default-directory dir)
      (setq mode-line-format (doom-modeline 'scratch)))))


;;
;; Global Defuns
;;

(defsubst --subdirs (path &optional include-self)
  "Get list of subdirectories in PATH, including PATH is INCLUDE-SELF is
non-nil."
  (let ((result (if include-self (list path) (list))))
    (mapc (lambda (file)
            (when (file-directory-p file)
              (push file result)))
          (ignore-errors (directory-files path t "^[^.]" t)))
    result))

(defun doom-reload ()
  "Reload `load-path' and `custom-theme-load-path', in case you updated cask
while emacs was open!"
  (interactive)
  (let ((-load-path
         (append (list doom-private-dir doom-core-dir doom-modules-dir doom-packages-dir)
                 (--subdirs doom-core-dir t)
                 (--subdirs doom-modules-dir t)
                 (--subdirs doom-packages-dir)
                 (--subdirs (expand-file-name (format "../../%s/bootstrap" emacs-version)
                                              doom-packages-dir))
                 doom--load-path))
        (-custom-theme-load-path
         (append (--subdirs doom-themes-dir t)
                 custom-theme-load-path)))
    (setq load-path -load-path
          custom-theme-load-path -custom-theme-load-path)
    (if (called-interactively-p 'interactive)
        (message "Reloaded!")
      (list -load-path -custom-theme-load-path))))

(defun doom-reload-autoloads ()
  "Regenerate and reload autoloads.el."
  (interactive)
  (let ((generated-autoload-file (concat doom-core-dir "/autoloads.el")))
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file)
      (message "Deleted old autoloads.el"))
    (mapc (lambda (dir)
            (update-directory-autoloads (concat dir "/defuns"))
            (message "Scanned: %s" dir))
          (list doom-core-dir doom-modules-dir))
    (when (called-interactively-p 'interactive)
      (load "autoloads"))
    (message "Done!")))

(defun doom-byte-compile (&optional minimal)
  "Byte compile the core and library .el files in ~/.emacs.d. If MINIMAL is nil,
only byte compile a few important files. If t, compile all files too. If 'basic,
only compile defun libraries."
  (interactive)
  (mapc (lambda (f) (byte-compile-file (concat doom-emacs-dir "/" f) t))
        '("init.el" "core/core.el" "core/core-defuns.el" "core/core-ui.el"
          "core/core-modeline.el" "core/core-os.el" "core/core-os-osx.el"
          "core/core-os-win32.el" "core/core-os-linux.el"
          "private/my-commands.el" "private/my-bindings.el"))
  (unless (eq minimal 'basic)
    (unless minimal
      (byte-recompile-directory doom-core-dir 0 t)
      (byte-recompile-directory doom-modules-dir 0 t))
    (when minimal
      (byte-recompile-directory (concat doom-core-dir "/defuns") 0 t)
      (byte-recompile-directory (concat doom-modules-dir "/defuns") 0 t)))
  (message "Compiled!"))

(provide 'core-defuns)
;;; core-defuns.el ends here
