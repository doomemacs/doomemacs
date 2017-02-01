
;;;###autoload
(defun +evil*ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME. Modified to include other substitution
flags."
  ;; TODO Generalize this so I can offer it upstream
  (let ((regexp (concat "\\(?:^\\|[^\\\\]\\)"
                        "\\([#%@]\\)"
                        "\\(\\(?::\\(?:[phtreS~.]\\|g?s[^: $]+\\)\\)*\\)"))
        case-fold-search)
    (dolist (match (s-match-strings-all regexp file-name))
      (let ((flags (split-string (caddr match) ":" t))
            (path (file-relative-name
                   (pcase (cadr match)
                     ("@" (doom-project-root))
                     ("%" (buffer-file-name))
                     ("#" (and (other-buffer) (buffer-file-name (other-buffer)))))
                   default-directory))
            flag global)
        (when path
          (while flags
            (setq flag (pop flags))
            (when (string-suffix-p "\\" flag)
              (setq flag (concat flag (pop flags))))
            (when (string-prefix-p "gs" flag)
              (setq global t
                    flag (string-remove-prefix "g" flag)))
            (setq path
                  (or (pcase (substring flag 0 1)
                        ("p" (expand-file-name path))
                        ("~" (file-relative-name path "~"))
                        ("." (file-relative-name path default-directory))
                        ("h" (directory-file-name path))
                        ("t" (file-name-nondirectory (directory-file-name path)))
                        ("r" (file-name-sans-extension path))
                        ("e" (file-name-extension path))
                        ("s" (let* ((args (evil-delimited-arguments (substring flag 1) 2))
                                    (pattern (evil-transform-vim-style-regexp (car args)))
                                    (replace (cadr args)))
                               (replace-regexp-in-string
                                (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                (evil-transform-vim-style-regexp replace) path t t
                                (unless global 1))))
                        ("S" (shell-quote-argument path))
                        (_ path))
                      "")))
          (setq file-name
                (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                  (s-trim-left (car match)))
                                          path file-name t t 1)))))
    (setq file-name (replace-regexp-in-string regexp "\\1" file-name t))))


;;
;; Custom argument handlers
;;

(defvar +evil--buffer-match-global evil-ex-substitute-global "")

(defun +evil--ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-substitute-matches)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq flag 'update))

     ((eq flag 'stop)
      (evil-ex-delete-hl name)))))

(defun +evil--ex-buffer-match (arg &optional hl-name flags beg end)
  (when (and (eq flag 'update)
             evil-ex-substitute-highlight-all
             (not (zerop (length arg))))
    (condition-case lossage
        (let ((pattern (evil-ex-make-substitute-pattern
                        (if evil-ex-bang (regexp-quote arg) arg)
                        (or flags (list))))
              (range (or (evil-copy-range evil-ex-range)
                         (evil-range (or beg (line-beginning-position))
                                     (or end (line-end-position))
                                     'line
                                     :expanded t))))
          (evil-expand-range range)
          (evil-ex-hl-set-region hl-name
                                 (max (evil-range-beginning range) (window-start))
                                 (min (evil-range-end range) (window-end)))
          (evil-ex-hl-change hl-name pattern))
      (end-of-file
       (evil-ex-pattern-update-ex-info nil "incomplete replacement"))
      (user-error
       (evil-ex-pattern-update-ex-info nil (format "?%s" lossage))))))

;;;###autoload
(defun +evil-ex-buffer-match (flag &optional arg)
  (let ((hl-name 'evil-ex-buffer-match))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (+evil--ex-buffer-match arg hl-name (list (if +evil--buffer-match-global ?g))))))

;;;###autoload
(defun +evil-ex-global-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-match))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (let ((result (car-safe (evil-ex-parse-global arg))))
        (+evil--ex-buffer-match result hl-name nil (point-min) (point-max))))))

;;;###autoload
(defun +evil-window-move (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
evil-window-move-* (e.g. `evil-window-move-far-left')"
  (let* ((this-window (get-buffer-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (doom-popup-p that-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (case direction
                   ('left 'evil-window-move-far-left)
                   ('right 'evil-window-move-far-right)
                   ('up 'evil-window-move-very-top)
                   ('down 'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil (cond ((eq direction 'up) 'above)
                                                  ((eq direction 'down) 'below)
                                                  (t direction))))
        (with-selected-window that-window
          (switch-to-buffer doom-buffer))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

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
        (defer   (if (boundp 'defer) defer))
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
                     (push +evil-leader rest))
                    ((eq key :localleader)
                     (push +evil-localleader rest)))
          (setq key :prefix))
        (pcase key
          (:prefix  (setq prefix (concat prefix (kbd (pop rest)))))
          (:map     (setq keymaps (-list (pop rest))))
          (:map*    (setq defer t keymaps (-list (pop rest))))
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
        (when (stringp key)
          (setq key (kbd key)))
        (when prefix
          (setq key (cond ((vectorp key) (vconcat prefix key))
                          (t (concat prefix key)))))
        (unless (> (length rest) 0)
          (user-error "Map has no definition for %s" key))
        (setq def (pop rest))
        (push
         (cond ((and keymaps states)
                (macroexp-progn
                 (mapcar (lambda (keymap)
                           `(,(if defer 'evil-define-key 'evil-define-key*)
                             ',states ,keymap ,key ,def))
                         keymaps)))
               (keymaps
                (macroexp-progn
                 (mapcar (lambda (keymap)
                           `(define-key ,keymap ,key ,def))
                         keymaps)))
               (states
                (macroexp-progn
                 (mapcar (lambda (state)
                           `(define-key
                              (evil-state-property ',state ,(if local :local-keymap :keymap) t)
                              ,key ,def))
                         states)))
               (t `(,(if local 'local-set-key 'global-set-key)
                    ,key ,def)))
         forms)
        (setq states '()
              local nil))

       (t (user-error "Invalid key %s" key))))
    (macroexp-progn (reverse forms))))

(when noninteractive
  (defmacro map! (&rest rest)))
