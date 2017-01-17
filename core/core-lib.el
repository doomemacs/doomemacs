;;; core-lib.el

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
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

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
    (mapc (lambda (f)
            (let ((func (if (symbolp f) `(quote ,f) `(lambda (&rest _) ,@func-or-forms))))
              (mapc (lambda (h)
                      (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
                    (-list hook))))
          funcs)
    (macroexp-progn forms)))

;; TODO
(defmacro add-lambda-hook! (hooks func-name &rest forms))

;; TODO
(defmacro remove-hooks! (hooks &rest funcs))

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
                                            (doom-project-has-files ,@(-list files))))
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

;; Register keywords for proper indentation (see `map!')
(put ':prefix       'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':map*         'lisp-indent-function 'defun)
(put ':after        'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)

(defmacro map! (&rest rest)
  "A nightmare of a key-binding macro that will use `evil-define-key',
`evil-define-key*', `define-key' and `global-set-key' depending on context and
plist key flags. It was designed to make binding multiple keys more concise,
like in vim.

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
    (:map [KEYMAP] [...])      ; apply inner keybinds to KEYMAP
    (:map* [KEYMAP] [...])     ; apply inner keybinds to KEYMAP (deferred)
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
        (defer (if (boundp 'defer) defer))
        (prefix (if (boundp 'prefix) prefix))
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
      (push
       (reverse
        (cond
         ;; it's a sub expr
         ((listp key)
          `(,(macroexpand `(map! ,@key))))

         ;; it's a flag
         ((keywordp key)
          (when (cond ((eq key :leader)
                       (push doom-evil-leader rest))
                      ((eq key :localleader)
                       (push doom-evil-localleader rest)))
            (setq key :prefix))
          (pcase key
            (:prefix  (setq prefix (concat prefix (kbd (pop rest)))) nil)
            (:map     (setq keymaps (-list (pop rest))) nil)
            (:map*    (setq defer t keymaps (-list (pop rest))) nil)
            (:unset  `(,(macroexpand `(map! ,(kbd (pop rest)) nil))))
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
                      (user-error "local keybinding for %s cannot accompany a keymap" key))))
             nil)))

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
            (cond ((and keymaps states)
                   (mapc (lambda (keymap)
                           (push `(,(if defer 'evil-define-key 'evil-define-key*)
                                   ',states ,keymap ,key ,def)
                                 out-forms))
                         keymaps))
                  (keymaps
                   (mapc (lambda (keymap) (push `(define-key ,keymap ,key ,def) out-forms))
                         keymaps))
                  (states
                   (mapc (lambda (state)
                           (push `(define-key
                                    (evil-state-property ',state ,(if local :local-keymap :keymap) t)
                                    ,key ,def)
                                 out-forms))
                         states))
                  (t (push `(,(if local 'local-set-key 'global-set-key)
                             ,key ,def)
                           out-forms)))
            (setq states '()
                  local nil)
            out-forms))

         (t (user-error "Invalid key %s" key))))
       forms))
    `(progn ,@(apply #'nconc (delete nil (delete (list nil) (reverse forms)))))))

(provide 'core-lib)
;;; core-lib.el ends here
