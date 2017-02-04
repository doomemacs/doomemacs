;;; core-lib.el

(defvar +evil-leader)
(defvar +evil-localleader)

(defmacro λ! (&rest body)
  "A shortcut for inline keybind lambdas."
  `(lambda () (interactive) ,@body))

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
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms
will be wrapped in a lambda. A list of symbols will expand into a series of
add-hook calls.

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

(defmacro associate! (mode &rest plist)
  "Associate a major or minor mode to certain patterns and project files."
  (declare (indent 1))
  (let* ((minor (plist-get plist :minor))
         (in    (plist-get plist :in))
         (match (plist-get plist :match))
         (files (plist-get plist :files))
         (pred  (plist-get plist :when)))
    (cond ((or files in pred)
           (when (and files (not (or (listp files) (stringp files))))
             (user-error "associate! :files expects a string or list of strings"))
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
          (t (user-error "associate! invalid rules for mode [%s] (in %s) (match %s) (files %s)"
                         mode in match files)))))

;; Register keywords for proper indentation (see `map!')
(put ':prefix       'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':map*         'lisp-indent-function 'defun)
(put ':after        'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)

;;;###autoload
(defmacro map! (&rest rest)
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
    :unset [KEY]               ; unset key
    (:map [KEYMAP] [...])      ; inner keybinds are applied to KEYMAP
    (:prefix [PREFIX] [...])   ; assign prefix to all inner keybindings
    (:after [FEATURE] [...])   ; apply keybinds when [FEATURE] loads

Conditional keybinds
    (:when [CONDITION] [...])
    (:unless [CONDITION] [...])

Example
    (map! :map magit-mode-map
          :m \"C-r\" 'do-something           ; assign C-r in motion state
          :nv \"q\" 'magit-mode-quit-window  ; assign to 'q' in normal and visual states
          \"C-x C-r\" 'a-global-keybind

          (:when IS-MAC
           :n \"M-s\" 'some-fn
           :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
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
        (push (macroexpand `(map! ,@key)) forms))

       ;; it's a flag
       ((keywordp key)
        (when (cond ((eq key :leader)
                     (push (or +evil-leader ",") rest))
                    ((eq key :localleader)
                     (push (or +evil-localleader "\\") rest)))
          (setq key :prefix))
        (pcase key
          (:prefix  (setq prefix (concat prefix (kbd (pop rest)))))
          (:map     (setq keymaps (-list (pop rest))))
          (:unset  `(,(macroexpand `(map! ,(kbd (pop rest))))))
          (:after   (prog1 `((after! ,(pop rest)   ,(macroexpand `(map! ,@rest)))) (setq rest '())))
          (:when    (prog1 `((if ,(pop rest)       ,(macroexpand `(map! ,@rest)))) (setq rest '())))
          (:unless  (prog1 `((if (not ,(pop rest)) ,(macroexpand `(map! ,@rest)))) (setq rest '())))
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
           (when (assoc "L" states)
             (cond ((= (length states) 1)
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
                (setq key (if (vectorp key) (vconcat prefix key) (concat prefix key))))
              (unless (> (length rest) 0)
                (user-error "Map has no definition for %s" key))
              (setq def (pop rest))
              (push
               (cond ((and keymaps states)
                      (throw 'skip 'evil)
                      (macroexp-progn
                       (mapcar (lambda (keymap) `(evil-define-key* ',states ,keymap ,key ,def))
                               keymaps)))
                     (keymaps
                      (macroexp-progn
                       (mapcar (lambda (keymap) `(define-key ,keymap ,key ,def))
                               keymaps)))
                     (states
                      (throw 'skip 'evil)
                      (macroexp-progn
                       (mapcar (lambda (state)
                                 `(define-key
                                    (evil-state-property ',state ,(if local :local-keymap :keymap) t)
                                    ,key ,def))
                               states)))
                     (t `(,(if local 'local-set-key 'global-set-key)
                          ,key ,def)))
               forms))
          (setq states '()
                local nil)))

       (t (user-error "Invalid key %s" key))))
    (macroexp-progn (reverse forms))))

(when (or noninteractive doom-dont-load-p)
  (defmacro add-hook! (&rest _))
  (defmacro associate! (&rest _))
  (defmacro map! (&rest _)))

(provide 'core-lib)
;;; core-lib.el ends here
