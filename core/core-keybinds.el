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
  (add-hook 'window-setup-hook #'which-key-mode))


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


(defun doom--keyword-to-states (keyword &optional ignore)
  "Convert a KEYWORD into a list of evil state symbols.

IGNORE is a list of keyword letters that should be ignored.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`doom-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (cdr (assq l doom-evil-state-alist))
             collect it
           else if (not (or (eq ignore t) (member l ignore)))
             do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put ':prefix       'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':map*         'lisp-indent-function 'defun)
(put ':mode         'lisp-indent-function 'defun)
(put ':after        'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':desc         'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)
(put ':textobj      'lisp-indent-function 'defun)

;; specials
(defvar doom--keymaps nil)
(defvar doom--prefix  nil)
(defvar doom--defer   nil)

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
    :L  local

    These can be combined (order doesn't matter), e.g. :nvi will apply to
    normal, visual and insert mode. The state resets after the following
    key=>def pair.

    If states are omitted the keybind will be global.

    This can be customized with `doom-evil-state-alist'.

    :L cannot be in a :map.

    :textobj is a special state that takes a key and two commands, one for the
    inner binding, another for the outer.

Flags
    (:mode [MODE(s)] [...])    ; inner keybinds are applied to major MODE(s)
    (:map [KEYMAP(s)] [...])   ; inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP(s)] [...])  ; same as :map, but deferred
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
  (let ((doom--keymaps doom--keymaps)
        (doom--prefix  doom--prefix)
        (doom--defer   doom--defer)
        local key def states forms desc modes)
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
            (setq doom--keymaps
                  (let ((car (pop rest)))
                    (if (listp car) car (list car)))))
          (:mode
            (setq modes
                  (let ((car (pop rest)))
                    (if (listp car) car (list car))))
            (unless doom--keymaps
              (setq doom--keymaps
                    (cl-loop for m in modes
                             collect (intern (format "%s-map" (symbol-name m)))))))
          (:textobj
            (let* ((key (pop rest))
                   (inner (pop rest))
                   (outer (pop rest)))
              (push (macroexpand `(map! (:map evil-outer-text-objects-map ,key ,inner)
                                        (:map evil-inner-text-objects-map ,key ,outer)))
                    forms)))
          (:prefix
            (let ((def (pop rest)))
              (setq doom--prefix `(vconcat ,doom--prefix (kbd ,def)))
              (when desc
                (push `(doom--keybind-register ,(key-description (eval doom--prefix))
                                               ,desc ',modes)
                      forms)
                (setq desc nil))))
          (_ ; might be a state doom--prefix
           (setq states (doom--keyword-to-states key '("L")))
           (let (case-fold-search)
             (when (string-match-p "L" (symbol-name key))
               (setq local t)
               (cond ((= (length states) 0)
                      (user-error "local keybinding for %s must accompany another state" key))
                     ((> (length doom--keymaps) 0)
                      (user-error "local keybinding for %s cannot accompany a keymap" key))))))))

       ;; It's a key-def pair
       ((or (stringp key)
            (characterp key)
            (vectorp key)
            (symbolp key))
        (unwind-protect
            (catch 'skip
              (cond ((symbolp key)
                     (setq key `(kbd ,key)))
                    ((stringp key)
                     (setq key (kbd key))))
              (when doom--prefix
                (setq key (append doom--prefix (list key))))
              (unless (> (length rest) 0)
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when desc
                (push `(doom--keybind-register ,(key-description (eval key))
                                              ,desc ',modes)
                      forms))
              (cond ((and doom--keymaps states)
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (keymap doom--keymaps)
                       (push `(,(if doom--defer 'evil-define-key 'evil-define-key*)
                               ',states ,keymap ,key ,def)
                             forms)))
                    (states
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (state states)
                       (push `(define-key
                                ,(intern (format "evil-%s-state-%smap" state (if local "local-" "")))
                                ,key ,def)
                             forms)))
                    (doom--keymaps
                     (dolist (keymap doom--keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t
                     (push `(,(if local 'local-set-key 'global-set-key) ,key ,def)
                           forms))))
          (setq states '()
                local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
    `(progn ,@(nreverse forms))))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
