;; Convenience ;;;;;;;;;;;;;;;;;;;;;
(defun associate-mode (match mode)
  (add-to-list 'auto-mode-alist (cons match mode)))

(defun associate-minor-mode (match mode)
  (add-to-list 'auto-minor-mode-alist (cons match mode)))

;; Automatic minor modes ;;;;;;;;;;;
(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist' All elements of this alist are checked, meaning
you can enable multiple minor modes for the same regexp.")
(defun enable-minor-mode-based-on-extension ()
  "check file name against auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))
(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)

(defmacro λ (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro add-hook! (hook &rest body)
  `(add-hook ,hook (lambda() ,@body)))

;; Backwards compatibility
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

(after "projectile"
  (defun my--project-root (&optional force-pwd)
    (if (and (not force-pwd)
             (projectile-project-p))
        (projectile-project-root)
      default-directory)))


;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bind (state &rest keys)
  (let ((state-list state)
        (is-global (or (stringp state)
                       (vectorp state)))
        keymap)
    (if is-global
        (setq keys (-insert-at 0 state keys))
      (progn
        (if (keymapp (car keys))
            (setq keymap (pop keys)))
        (if (or (keymapp state)
                (not (listp state)))
            (setq state-list (list state)))))
    (while keys
      (let ((-key (pop keys))
            (-def (pop keys)))
        (if (stringp -key)
            (setq -key (kbd -key)))
        (if is-global
            (global-set-key -key -def)
          (dolist (-state state-list)
            (cond ((evil-state-p -state)
                   (define-key
                     (if keymap
                         (evil-get-auxiliary-keymap keymap -state t)
                       (evil-state-property -state :keymap t)) -key -def))
                  ((keymapp -state)
                   (define-key -state -key -def)))))))))

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

(defun disable-final-newline ()
  (set (make-local-variable 'require-final-newline) nil))

(defun load-init-files ()
  ;; (mapc 'require io-modules)
  (dolist (module my-modules)
    (message "%s" (symbol-name module))
    (with-demoted-errors "#### ERROR: %s"
      (require module))))


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


(provide 'defuns)
