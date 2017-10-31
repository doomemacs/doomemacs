;;; core-lib.el -*- lexical-binding: t; -*-

;; I don't use use-package for these to save on the `fboundp' lookups it does
;; for its :commands property. I use dolists instead of mapc to avoid extra
;; stackframes allocated for lambdas. This is _definitely_ premature
;; optimization.
(dolist (sym '(async-start async-start-process async-byte-recompile-directory
               async-inject-variables))
  (autoload sym "async"))

(dolist (sym '(persistent-soft-exists-p persistent-soft-fetch
               persistent-soft-flush persistent-soft-store))
  (autoload sym "persistent-soft"))

(dolist (sym '(s-center s-pad-left s-pad-right s-truncate s-chop-suffix
               s-chop-suffixes s-chop-prefix s-chop-prefixes s-join s-replace
               s-replace-all s-capitalize s-titleize s-split-words
               s-capitalized-words s-titleized-words))
  (autoload sym "s"))

(dolist (sym '(when-let if-let string-trim string-join string-blank-p string-lessp))
  (autoload sym "subr-x" nil nil 'macro))

(dolist (sym '(json-read json-read-file json-read-from-string json-encode))
  (autoload sym "json"))


;;
;; Helpers
;;

;; stole from https://emacs.stackexchange.com/a/16495/16662
(defmacro doom-with-advice (args &rest body)
  (declare (indent 1))
  (let ((fun-name (car args))
        (advice   (cadr args))
        (orig-sym (make-symbol "orig")))
    `(cl-letf* ((,orig-sym  (symbol-function ',fun-name))
                ((symbol-function ',fun-name)
                 (lambda (&rest args)
                   (apply ,advice ,orig-sym args))))
       ,@body)))

(defun doom--resolve-path-forms (paths &optional root)
  (cond ((stringp paths)
         `(file-exists-p
           (expand-file-name
            ,paths ,(if (or (string-prefix-p "./" paths)
                            (string-prefix-p "../" paths))
                        'default-directory
                      (or root `(doom-project-root))))))
        ((listp paths)
         (cl-loop for i in paths
                  collect (doom--resolve-path-forms i root)))
        (t paths)))

(defun doom--resolve-hook-forms (hooks)
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (doom-enlist (doom-unquote hooks))
           if (eq (car-safe hook) 'quote)
            collect (cadr hook)
           else if quoted-p
            collect hook
           else collect (intern (format "%s-hook" (symbol-name hook)))))

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (if (listp exp) exp (list exp)))

(defun doom-resolve-vim-path (file-name)
  "Take a path and resolve any vim-like filename modifiers in it. This adds
support for these special modifiers:

  %:P   Resolves to `doom-project-root'.

See http://vimdoc.sourceforge.net/htmldoc/cmdline.html#filename-modifiers."
  (let* (case-fold-search
         (regexp (concat "\\(?:^\\|[^\\\\]\\)"
                         "\\([#%]\\)"
                         "\\(\\(?::\\(?:[PphtreS~.]\\|g?s[^:\t\n ]+\\)\\)*\\)"))
         (matches
          (cl-loop with i = 0
                   while (and (< i (length file-name))
                              (string-match regexp file-name i))
                   do (setq i (1+ (match-beginning 0)))
                   and collect
                   (cl-loop for j to (/ (length (match-data)) 2)
                            collect (match-string j file-name)))))
    (dolist (match matches)
      (let ((flags (split-string (car (cdr (cdr match))) ":" t))
            (path (and buffer-file-name
                       (pcase (car (cdr match))
                         ("%" (file-relative-name buffer-file-name))
                         ("#" (save-excursion (other-window 1) (file-relative-name buffer-file-name))))))
            flag global)
        (if (not path)
            (setq path "")
          (while flags
            (setq flag (pop flags))
            (when (string-suffix-p "\\" flag)
              (setq flag (concat flag (pop flags))))
            (when (string-prefix-p "gs" flag)
              (setq global t
                    flag (substring flag 1)))
            (setq path
                  (or (pcase (substring flag 0 1)
                        ("p" (expand-file-name path))
                        ("~" (concat "~/" (file-relative-name path "~")))
                        ("." (file-relative-name path default-directory))
                        ("t" (file-name-nondirectory (directory-file-name path)))
                        ("r" (file-name-sans-extension path))
                        ("e" (file-name-extension path))
                        ("S" (shell-quote-argument path))
                        ("h"
                         (let ((parent (file-name-directory (expand-file-name path))))
                           (unless (equal (file-truename path)
                                          (file-truename parent))
                             (if (file-name-absolute-p path)
                                 (directory-file-name parent)
                               (file-relative-name parent)))))
                        ("s"
                         (if (featurep 'evil)
                             (when-let (args (evil-delimited-arguments (substring flag 1) 2))
                               (let ((pattern (evil-transform-vim-style-regexp (car args)))
                                     (replace (cadr args)))
                                 (replace-regexp-in-string
                                  (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                  (evil-transform-vim-style-regexp replace) path t t
                                  (unless global 1))))
                           path))
                        ("P"
                         (let ((default-directory (file-name-directory (expand-file-name path))))
                           (abbreviate-file-name (doom-project-root))))
                        (_ path))
                      "")))
          ;; strip trailing slash, if applicable
          (when (and (not (string= path "")) (equal (substring path -1) "/"))
            (setq path (substring path 0 -1))))
        (setq file-name
              (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                (regexp-quote (string-trim-left (car match))))
                                        path file-name t t 1))))
    (replace-regexp-in-string regexp "\\1" file-name t)))


;;
;; Library
;;

(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if doom-debug-mode
       (progn ,@forms)
     (fset 'doom--old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((standard-output (lambda (&rest _)))
               ((symbol-function 'load-file) (lambda (file) (load file nil t)))
               ((symbol-function 'message) (lambda (&rest _)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (doom--old-write-region-fn
                   start end filename append visit lockname mustbenew)))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))

(defvar doom--transient-counter 0)
(defmacro add-transient-hook! (hook &rest forms)
  "Attaches transient forms to a HOOK.

HOOK can be a quoted hook or a sharp-quoted function (which will be advised).

These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
  (declare (indent 1))
  (let ((append (eq (car forms) :after))
        (fn (intern (format "doom-transient-hook-%s" (cl-incf doom--transient-counter)))))
    `(when ,hook
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook) (advice-remove ,hook #',fn))
                     ((symbolp ,hook)   (remove-hook ,hook #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook)
              (advice-add ,hook ,(if append :after :before) #',fn))
             ((symbolp ,hook)
              (add-hook ,hook #',fn ,append))))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (doom--resolve-hook-forms (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (cond ((eq hook-fn 'remove-hook)
                       `(remove-hook ',hook ,fn ,local-p))
                      (t
                       `(add-hook ',hook ,fn ,append-p ,local-p)))
                forms)))
      `(progn ,@(nreverse forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'. Takes the same arguments as
`add-hook!'."
  `(add-hook! :remove ,@args))

(defmacro associate! (mode &rest plist)
  "Associate a minor mode to certain patterns and project files."
  (declare (indent 1))
  (unless noninteractive
    (let ((modes (plist-get plist :modes))
          (match (plist-get plist :match))
          (files (plist-get plist :files))
          (pred-form (plist-get plist :when)))
      (cond ((or files modes pred-form)
             (when (and files
                        (not (or (listp files)
                                 (stringp files))))
               (user-error "associate! :files expects a string or list of strings"))
             (let ((hook-name (intern (format "doom--init-mode-%s" mode))))
               `(progn
                  (defun ,hook-name ()
                    (when (and (boundp ',mode)
                               (not ,mode)
                               (and buffer-file-name (not (file-remote-p buffer-file-name)))
                               ,(if match `(if buffer-file-name (string-match-p ,match buffer-file-name)) t)
                               ,(if files (doom--resolve-path-forms files) t)
                               ,(or pred-form t))
                      (,mode 1)))
                  ,@(if (and modes (listp modes))
                        (cl-loop for hook in (doom--resolve-hook-forms modes)
                                 collect `(add-hook ',hook ',hook-name))
                      `((add-hook 'after-change-major-mode-hook ',hook-name))))))
            (match
             `(push (cons ,match ',mode) doom-auto-minor-mode-alist))
            (t (user-error "associate! invalid rules for mode [%s] (modes %s) (match %s) (files %s)"
                           mode modes match files))))))


;; I'm a fan of concise, hassle-free front-facing configuration. Rather than
;; littering my config with `after!' blocks, and checking if features and
;; modules are loaded before every line of config, I wrote `set!' as a more
;; robust alternative. If a setting doesn't exist at run-time, the `set!' call
;; is ignored. It also benefits from byte-compilation.
(defvar doom-settings nil)

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting macro. Like `defmacro', this should return a form to be
executed when called with `set!'. FORMS are not evaluated until `set!' calls it."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  (let ((fn (intern (format "doom--set%s" keyword))))
    `(progn
       (defun ,fn ,arglist
         ,docstring
         ,@forms)
       (cl-pushnew ',(cons keyword fn) doom-settings :test #'eq :key #'car))))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if doesn't exist."
  (declare (indent defun))
  (unless values
    (error "Empty set! for %s" keyword))
  (let ((fn (cdr (assq keyword doom-settings))))
    (if fn
        (apply fn values)
      (when doom-debug-mode
        (message "No setting found for %s" keyword)
        nil))))

(provide 'core-lib)
;;; core-lib.el ends here
