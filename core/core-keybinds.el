;;; core-keybinds.el

;; A centralized keybinds system. Uses `which-key' to preview the available keys
;; when using `doom-leader-key' or `doom-localleader-key'.

(defvar doom-leader-key ","
  "The leader prefix key, for global commands.")

(defvar doom-localleader-key "\\"
  "The leader prefix key, for global commands.")

(defvar doom--which-key-defs '(nil))


(def-package! which-key
  :defer 1
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 10
        ;; only pop-up for leader/localleader keys
        which-key-allow-regexps (list (format "^%s" (regexp-quote doom-leader-key))
                                      (format "^%s" (regexp-quote doom-localleader-key))))
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (which-key-mode +1)
  (when (cdr doom--which-key-defs)
    (apply 'which-key-add-key-based-replacements (cdr doom--which-key-defs))))


(defun doom-keyword-to-states (keyword &optional ignore)
  (let ((states '(("n" . normal)
                  ("v" . visual)
                  ("i" . insert)
                  ("e" . emacs)
                  ("o" . operator)
                  ("m" . motion)
                  ("r" . replace))))
    (delq nil
          (mapcar (lambda (l)
                    (let ((state (cdr (assoc l states))))
                      (unless state
                        (unless (or (eq ignore t) (member l ignore))
                          (error "not a valid state: %s" l)))
                      state))
                  (split-string (substring (symbol-name keyword) 1) "" t)))))

;; Register keywords for proper indentation (see `map!')
(put ':prefix       'lisp-indent-function 'defun)
(put ':map          'lisp-indent-function 'defun)
(put ':map*         'lisp-indent-function 'defun)
(put ':after        'lisp-indent-function 'defun)
(put ':when         'lisp-indent-function 'defun)
(put ':unless       'lisp-indent-function 'defun)
(put ':desc         'lisp-indent-function 'defun)
(put ':leader       'lisp-indent-function 'defun)
(put ':localleader  'lisp-indent-function 'defun)

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

    :L cannot be in a :map.

Flags
    (:map [KEYMAP] [...])      ; inner keybinds are applied to KEYMAP(S)
    (:map* [KEYMAP] [...])     ; same as :map, but deferred
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
        (defer   (if (boundp 'defer) defer))
        local key def states forms desc)
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
          (:when    (prog1 `((if ,(pop rest)       ,(macroexpand `(map! ,@rest)))) (setq rest '())))
          (:unless  (prog1 `((if (not ,(pop rest)) ,(macroexpand `(map! ,@rest)))) (setq rest '())))
          (:after   (prog1 `((after! ,(pop rest)   ,(macroexpand `(map! ,@rest)))) (setq rest '())))
          (:desc    (setq desc (pop rest)))
          (:map*    (setq defer t) (push :map rest))
          (:map
            (setq keymaps
                  (let ((car (pop rest)))
                    (if (listp car) car (list car)))))
          (:prefix
            (let ((def (pop rest)))
              (setq prefix
                    (if (or (symbolp def) (listp def))
                        `(vconcat ,prefix (if (stringp ,def) (kbd ,def) ,def))
                      `(vconcat ,prefix ,(if (stringp def) (kbd def) def))))
              (when desc
                (push `(nconc doom--which-key-defs (list ,(key-description (eval prefix)) ,desc))
                      forms)
                (setq desc nil))))
          (otherwise ; might be a state prefix
           (setq states (doom-keyword-to-states key '("L")))
           (when (let (case-fold-search)
                   (string-match-p "L" (symbol-name key)))
             (setq local t)
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
                (user-error "map! has no definition for %s key" key))
              (setq def (pop rest))
              (when desc
                (push `(nconc doom--which-key-defs (list ,(key-description (eval key)) ,desc))
                      forms))
              (cond ((and keymaps states)
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (keymap keymaps)
                       (push `(,(if defer 'evil-define-key 'evil-define-key*)
                               ',states ,keymap ,key ,def)
                             forms)))
                    (states
                     (unless (featurep 'evil) (throw 'skip 'evil))
                     (dolist (state states)
                       (push `(define-key
                                ,(intern (format "evil-%s-state-%smap" state (if local "local-" "")))
                                ,key ,def)
                             forms)))
                    (keymaps
                     (dolist (keymap keymaps)
                       (push `(define-key ,keymap ,key ,def) forms)))
                    (t (push `(,(if local 'local-set-key 'global-set-key) ,key ,def)
                             forms))))
          (setq states '()
                local nil
                desc nil)))

       (t (user-error "Invalid key %s" key))))
    `(progn ,@(nreverse forms))))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
