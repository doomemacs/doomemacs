;;; core-lib.el

(require 'dash)
(require 's)
(require 'f)

(defvar __DIR__ nil  "The directory of the currently loaded file (set by `@load')")
(defvar __FILE__ nil "The full path of the currently loaded file (set by `@load')")

(defun __DIR__ ()
  "Get the full path to the current file's parent folder."
  (or __DIR__
      (and load-file-name (f-dirname load-file-name))
      (and buffer-file-name (f-dirname buffer-file-name))
      default-directory
      (and (bound-and-true-p byte-compile-current-file)
           (f-dirname byte-compile-current-file))
      (error "__DIR__ is unset")))

(defun __FILE__ ()
  "Get the full path to the current file."
  (or __FILE__
      load-file-name
      buffer-file-name
      (and (bound-and-true-p byte-compile-current-file)
           byte-compile-current-file)
      (error "__FILE__ is unset")))

(@package anaphora
  :commands (awhen aif acond awhile))

(@package async
  :commands (async-start
             async-start-process
             async-byte-recompile-directory))

(@package persistent-soft
  :preface (defvar pcache-directory (concat doom-cache-dir "pcache/"))
  :commands (persistent-soft-exists-p
             persistent-soft-fetch
             persistent-soft-flush
             persistent-soft-store))


;;
;; Library
;;

(defmacro @λ (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defmacro @after (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro @quiet (&rest forms)
  "Run FORMS without making any noise."
  `(progn
     (fset 'doom--old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((standard-output (lambda (&rest _)))
               ((symbol-function 'load-file) (lambda (file) (load file nil t)))
               ((symbol-function 'message) (lambda (&rest _)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (doom--old-write-region-fn start end filename append visit lockname mustbenew)))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))

(defmacro @add-hook (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms
will be wrapped in a lambda. A list of symbols will expand into a series of
add-hook calls.

Examples:
    (@add-hook 'some-mode-hook 'enable-something)
    (@add-hook some-mode '(enable-something and-another))
    (@add-hook '(one-mode-hook second-mode-hook) 'enable-something)
    (@add-hook (one-mode second-mode) 'enable-something)
    (@add-hook (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "@add-hook: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted-p (eq (car-safe hook) 'quote))
         (hook (if quoted-p (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms))))
    (macroexp-progn
     (mapcar (lambda (f)
               (let ((func (if (symbolp f) `(quote ,f) `(lambda (&rest _) ,@func-or-forms))))
                 (macroexp-progn
                  (mapcar (lambda (h)
                            `(add-hook ',(if quoted-p h (intern (format "%s-hook" h))) ,func))
                          (-list hook)))))
             funcs))))

(defmacro @associate (mode &rest plist)
  "Associate a major or minor mode to certain patterns and project files."
  (declare (indent 1))
  (unless noninteractive
    (let* ((minor (plist-get plist :minor))
           (in    (plist-get plist :in))
           (match (plist-get plist :match))
           (files (plist-get plist :files))
           (pred  (plist-get plist :when)))
      (cond ((or files in pred)
             (when (and files (not (or (listp files) (stringp files))))
               (user-error "@associate :files expects a string or list of strings"))
             (let ((hook-name (intern (format "doom--init-mode-%s" mode))))
               (macroexp-progn
                (list `(defun ,hook-name ()
                         (when (and ,(if match `(if buffer-file-name (string-match-p ,match buffer-file-name)) t)
                                    (or ,(not files)
                                        (and (boundp ',mode)
                                             (not ,mode)
                                             (doom-project-has-files ,@(-list files))))
                                    (or (not ,pred)
                                        (funcall ,pred buffer-file-name)))
                           (,mode 1)))
                      (if (and in (listp in))
                          (macroexp-progn
                           (mapcar (lambda (h) `(add-hook ',h ',hook-name))
                                   (mapcar (lambda (m) (intern (format "%s-hook" m))) in)))
                        `(add-hook 'find-file-hook ',hook-name))))))
            (match
             `(add-to-list ',(if minor 'doom-auto-minor-mode-alist 'auto-mode-alist)
                           (cons ,match ',mode)))
            (t (user-error "@associate invalid rules for mode [%s] (in %s) (match %s) (files %s)"
                           mode in match files))))))

;; Register keywords for proper indentation (see `@map')
(put ':prefix       'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':after        'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)

(defmacro @map (&rest rest)
  "A nightmare of a key-binding macro that will use `evil-define-key*',
`define-key', `local-set-key' and `global-set-key' depending on context and
plist key flags. It was designed to make binding multiple keys more concise,
like in vim.

If evil isn't loaded, it will ignore evil-specific bindings.

Yes, it tries to do too much. Yes, I only did it to make the \"frontend\" config
that little bit more concise. Yes, I could simply have used the above functions.
But it takes a little insanity to custom write your own emacs.d, so what else
were you expecting?

States
    :n  normal
    :v  visual
    :i  insert
    :e  emacs
    :o  operator
    :m  motion
    :r  replace
    :L  local

    These can be combined (order doesn't matter), e.g. :nvi will apply to
    normal, visual and insert mode. The state resets after the following
    key=>def pair.

    Capitalize the state flag to make it a local binding.

    If omitted, the keybind will be defined globally.

Flags
    (:map [KEYMAP] [...])      ; inner keybinds are applied to KEYMAP
    (:prefix [PREFIX] [...])   ; assign prefix to all inner keybindings
    (:after [FEATURE] [...])   ; apply keybinds when [FEATURE] loads

Conditional keybinds
    (:when [CONDITION] [...])
    (:unless [CONDITION] [...])

Example
    (@map :map magit-mode-map
          :m \"C-r\" 'do-something           ; assign C-r in motion state
          :nv \"q\" 'magit-mode-quit-window  ; assign to 'q' in normal and visual states
          \"C-x C-r\" 'a-global-keybind

          (:when IS-MAC
           :n \"M-s\" 'some-fn
           :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (unless noninteractive
    (let ((keymaps (if (boundp 'keymaps) keymaps))
          (prefix  (if (boundp 'prefix) prefix))
          (state-map '(("n" . normal)
                       ("v" . visual)
                       ("i" . insert)
                       ("e" . emacs)
                       ("o" . operator)
                       ("m" . motion)
                       ("r" . replace)))
          local key def states forms)
      (while rest
        (setq key (pop rest))
        (cond
         ;; it's a sub expr
         ((listp key)
          (push (macroexpand `(@map ,@key)) forms))

         ;; it's a flag
         ((keywordp key)
          (when (memq key '(:leader :localleader))
            (if (not (featurep '+evil))
                (setq rest nil
                      key :ignore)
              (cond ((eq key :leader)
                     (push '+evil-leader rest))
                    ((eq key :localleader)
                     (push '+evil-localleader rest)))
              (setq key :prefix)))
          (pcase key
            (:ignore)
            (:prefix
              (let ((def (pop rest)))
                (setq prefix
                      (if (or (symbolp def) (listp def))
                          `(vconcat ,prefix (if (stringp ,def) (kbd ,def) ,def))
                        `(vconcat ,prefix ,(if (stringp def) (kbd def) def))))))
            (:map     (setq keymaps (-list (pop rest))))
            (:after   (prog1 `((@after ,(pop rest)   ,(macroexpand `(@map ,@rest)))) (setq rest '())))
            (:when    (prog1 `((if ,(pop rest)       ,(macroexpand `(@map ,@rest)))) (setq rest '())))
            (:unless  (prog1 `((if (not ,(pop rest)) ,(macroexpand `(@map ,@rest)))) (setq rest '())))
            (otherwise ; might be a state prefix
             (mapc (lambda (letter)
                     (cond ((assoc letter state-map)
                            (push (cdr (assoc letter state-map)) states))
                           ((string= letter "L")
                            (setq local t))
                           (t (user-error "Invalid mode prefix %s in key %s" letter key))))
                   (split-string (substring (symbol-name key) 1) "" t))
             (unless states
               (user-error "Unrecognized keyword %s" key))
             (when local
               (cond ((= (length states) 0)
                      (user-error "local keybinding for %s must accompany another state" key))
                     ((> (length keymaps) 0)
                      (user-error "local keybinding for %s cannot accompany a keymap" key)))))))

         ;; It's a key-def pair
         ((or (stringp key)
              (characterp key)
              (vectorp key))
          (unwind-protect
              (catch 'skip
                (when (stringp key)
                  (setq key (kbd key)))
                (when prefix
                  (setq key (append prefix (list key))))
                (unless (> (length rest) 0)
                  (user-error "Map has no definition for %s" key))
                (setq def (pop rest))
                (push (cond ((and keymaps states)
                             (unless (featurep 'evil)
                               (throw 'skip 'evil))
                             (macroexp-progn
                              (mapcar (lambda (keymap) `(evil-define-key* ',states ,keymap ,key ,def))
                                      keymaps)))
                            (keymaps
                             (macroexp-progn
                              (mapcar (lambda (keymap) `(define-key ,keymap ,key ,def))
                                      keymaps)))
                            (states
                             (unless (featurep 'evil)
                               (throw 'skip 'evil))
                             (macroexp-progn
                              (mapcar (lambda (state)
                                        `(define-key
                                           ,(intern (format "evil-%s-state-%smap" state (if local "local-" "")))
                                           ,key ,def))
                                      states)))
                            (t `(,(if local 'local-set-key 'global-set-key)
                                 ,key ,def)))
                      forms))
            (setq states '()
                  local nil)))

         (t (user-error "Invalid key %s" key))))
      (macroexp-progn (reverse forms)))))

(provide 'core-lib)
;;; core-lib.el ends here
