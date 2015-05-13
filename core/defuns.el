;; Convenience ;;;;;;;;;;;;;;;;;;;;;
(defun associate-mode (match mode)
  "Associate a major mode with a filepath through `auto-mode-alist'"
  (add-to-list 'auto-mode-alist (cons match mode)))

(defun associate-minor-mode (match mode)
  "Associate a minor mode with a filepath through `auto-minor-mode-alist'"
  (add-to-list 'auto-minor-mode-alist (cons match mode)))

(defmacro λ (&rest body)
  "A shortcut for: `(lambda () (interactive) ,@body)"
  `(lambda () (interactive) ,@body))

(defun add-hooks (hooks funs)
  "Add multiple hooks to multiple funs."
  (let ((funs (if (listp funs) funs (list funs)))
        (hooks (if (listp hooks) hooks (list hooks))))
    (dolist (hook hooks)
      (dolist (fun funs)
        (add-hook hook fun)))))

(defmacro add-hook! (hook &rest body)
  "A shortcut macro for `add-hook' that auto-wraps `body' in a lambda"
  `(add-hook ,hook (lambda() ,@body)))

;; Backwards compatible `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(defmacro after (feature &rest forms)
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))


;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bind (&rest keys)
  (let (state-list keymap key def)
    (while keys
      (setq key (pop keys))
      (cond ((keymapp key)
             (setq keymap key))
            ((or (evil-state-p key)
                 (and (listp key) (evil-state-p (car key))))
             (setq state-list key))
            (t
             (if (stringp key)
                 (setq key (kbd key)))
             (when (eq (length keys) 0)
               (user-error "No definition for '%s' keybinding" key))
             (setq def (pop keys))
             (if (null state-list)
                 (if (null keymap)
                     (global-set-key key def)
                   (define-key keymap key def))
               (unless (listp state-list)
                 (setq state-list (list state-list)))
               (dolist (state state-list)
                 (define-key (if keymap
                                 (evil-get-auxiliary-keymap keymap state t)
                               (evil-state-property state :keymap t)) key def))))))))

(after "evil"
  (evil-define-command my--maybe-exit-insert-mode ()
    "Exits insert mode using jk without the momentary pause caused by
key-chord-define."
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "j")
      (let ((evt (read-event nil nil 0.4)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?k))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events (list evt)))))))))

;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-comment-hard-wrap ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill))

(defun enable-tab-width-2 ()
  (setq tab-width 2 evil-shift-width 2))

(defun enable-tab-width-4 ()
  (setq tab-width 4 evil-shift-width 4))

(defun disable-final-newline ()
  (set (make-local-variable 'require-final-newline) nil))


;; Font Defuns ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cycle-font (&optional i)
  "Cycle between fonts specified in *fonts in init.el"
  (interactive)
  (if (numberp i)
      (setq my/cycle-font-i i)
    (if (>= my/cycle-font-i (1- (length *fonts)))
        (setq my/cycle-font-i 0)
      (cl-incf my/cycle-font-i)))
  (let ((font (nth my/cycle-font-i *fonts)))
    (message "Changing font to %s" (nth 1 (font-face-attributes font)))
    (set-frame-font font)))


;;;; Global Defuns ;;;;;;;;;;;;;;;;;;;;;
(defun my--minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my--line-at-click ()
  "Determine the line number at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos)))))

(defun my-select-linum (event)
  "Set point as *linum-mdown-line*"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (my--line-at-click))
  (evil-visual-line)
  (setq *linum-mdown-line*
        (line-number-at-pos)))

(defun my-select-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))


(provide 'defuns)
