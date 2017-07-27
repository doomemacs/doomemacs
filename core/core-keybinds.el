;;; core-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' will be ignored.

(defvar doom-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar doom-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar doom-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace))
  "A list of cons cells that map a letter to a evil state symbol.")


;;
(def-package! which-key
  :demand t
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'doom-init-hook #'which-key-mode))


;;
(defun doom--keybind-register (key desc &optional modes)
  "Register a description for KEY with `which-key' in MODES.

  KEYS should be a string in kbd format.
  DESC should be a string describing what KEY does.
  MODES should be a list of major mode symbols."
  (if modes
      (dolist (mode modes)
        (which-key-add-major-mode-key-based-replacements mode key desc))
    (which-key-add-key-based-replacements key desc)))


(defun doom--keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`doom-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l doom-evil-state-alist))
             collect it
           else
             do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put ':after        'lisp-indent-function 'defun)
(put ':desc         'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':local        'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':map*         'lisp-indent-function 'defun)
(put ':mode         'lisp-indent-function 'defun)
(put ':prefix       'lisp-indent-function 'defun)
(put ':textobj      'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)

;; specials
(defvar doom--keymaps nil)
(defvar doom--prefix  nil)
(defvar doom--defer   nil)
(defvar doom--local   nil)

(defmacro map! (&rest rest)
  "A nightmare of a key-binding macro that will use `evil-define-key*',
`define-key', `local-set-key' and `global-set-key' depending on context and
plist key flags (and whether evil is loaded or not). It was designed to make
binding multiple keys more concise, like in vim.

If evil isn't loaded, it will ignore evil-specific bindings.

States
    :n  normal
    :v  visual
    :i  insert
    :e  emacs
    :o  operator
    :m  motion
    :r  replace

    These can be combined (order doesn't matter), e.g. :nvi will apply to
    normal, visual and insert mode. The state resets after the following
    key=>def pair.

    If states are omitted the keybind will be global.

    This can be customized with `doom-evil-state-alist'.

    :textobj is a special state that takes a key and two commands, one for the
    inner binding, another for the outer.

Flags
    (:mode [MODE(s)] [...])    inner keybinds are applied to major MODE(s)
    (:map [KEYMAP(s)] [...])   inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP(s)] [...])  same as :map, but deferred
    (:prefix [PREFIX] [...])   assign prefix to all inner keybindings
    (:after [FEATURE] [...])   apply keybinds when [FEATURE] loads
    (:local [...])             make bindings buffer local; incompatible with keymaps!

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
  (let ((doom--keymaps doom--keymaps)
        (doom--prefix  doom--prefix)
        (doom--defer   doom--defer)
        (doom--local   doom--local)
        key def states forms desc modes)
    (while rest
      (setq key (pop rest))
      (cond
       ;; it's a sub expr
       ((listp key)
        (push (macroexpand `(map! ,@key)) forms))

       ;; it's a flag
       ((keywordp key)
        (cond ((eq key :leader)
               (push 'doom-leader-key rest)
               (setq key :prefix
                     desc "<leader>"))
              ((eq key :localleader)
               (push 'doom-localleader-key rest)
               (setq key :prefix
                     desc "<localleader>")))
        (pcase key
          (:when    (push `(if ,(pop rest)       ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:unless  (push `(if (not ,(pop rest)) ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:after   (push `(after! ,(pop rest)   ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
          (:desc    (setq desc (pop rest)))
          (:map*    (setq doom--defer t) (push :map rest))
          (:map
            (setq doom--keymaps (doom-enlist (pop rest))))
          (:mode
            (setq modes (doom-enlist (pop rest)))
            (unless doom--keymaps
              (setq doom--keymaps
                    (cl-loop for m in modes
                             collect (intern (format "%s-map" (symbol-name m)))))))
          (:textobj
            (let* ((key (pop rest))
                   (inner (pop rest))
                   (outer (pop rest)))
              (push (macroexpand `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                        (:map evil-outer-text-objects-map ,key ,outer)))
                    forms)))
          (:prefix
            (let ((def (pop rest)))
              (setq doom--prefix `(vconcat ,doom--prefix (kbd ,def)))
              (when desc
                (push `(doom--keybind-register ,(key-description (eval doom--prefix))
                                               ,desc ',modes)
                      forms)
                (setq desc nil))))
          (:local
           (setq doom--local t))
          (_ ; might be a state doom--prefix
           (setq states (doom--keyword-to-states key)))))

       ;; It's a key-def pair
       ((or (stringp key)
            (characterp key)
            (vectorp key)
            (symbolp key))
        (unwind-protect
            (catch 'skip
              (when (symbolp key)
                (setq key `(kbd ,key)))
              (when (stringp key)
                (setq key (kbd key)))
              (when doom--prefix
                (setq key (append doom--prefix (list key))))
              (unless (> (length rest) 0)
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when desc
                (push `(doom--keybind-register ,(key-description (eval key))
                                              ,desc ',modes)
                      forms))
              (cond ((and doom--local doom--keymaps)
                     (push `(lwarn 'doom-map :warning
                                   "Can't local bind '%s' key to a keymap; skipped"
                                   ,key)
                           forms)
                     (throw 'skip 'local))
                    ((and doom--keymaps states)
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (keymap doom--keymaps)
                       (push `(,(if doom--defer 'evil-define-key 'evil-define-key*)
                               ',states ,keymap ,key ,def)
                             forms)))
                    (states
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (state states)
                       (push `(define-key
                                ,(intern (format "evil-%s-state-%smap" state (if doom--local "local-" "")))
                                ,key ,def)
                             forms)))
                    (doom--keymaps
                     (dolist (keymap doom--keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t
                     (push `(,(if doom--local 'local-set-key 'global-set-key) ,key ,def)
                           forms))))
          (setq states '()
                doom--local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
    `(progn ,@(nreverse forms))))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
