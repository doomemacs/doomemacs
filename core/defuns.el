(require 's)
(require 'dash)
(require 'f)

;; Compatibility ;;;;;;;;;;;;;;;;;;;
;; Backwards compatible `with-eval-after-load'
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(defmacro after (feature &rest forms)
  (declare (indent defun))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro shut-up (&rest body)
    "Silence message output from code."
    (declare (indent defun))
    `(let (message-log-max)
       ,@body
       (message "")))

(eval-when-compile
  ;; Convenience ;;;;;;;;;;;;;;;;;;;;;
  (defmacro λ (&rest body)
    "A shortcut for: `(lambda () (interactive) ,@body)"
    `(lambda () (interactive) ,@body))

  (defmacro associate-mode (match mode)
    "Associate a major mode with a filepath through `auto-mode-alist'"
    `(add-to-list 'auto-mode-alist (cons ,match ,mode)))

  (defmacro associate-minor-mode (match mode)
    "Associate a minor mode with a filepath through `auto-minor-mode-alist'"
    `(add-to-list 'narf/auto-minor-mode-alist (cons ,match ,mode)))

  ;; (defmacro add-to-mode (mode funcs))
  ;; (defmacro add-to-modes (func modes)
  ;;   (add-to-hooks))

  (defmacro add-to-hook (hook funcs)
    "Add a series of FUNCS to a hook. FUNCS can be a list."
    (declare (indent 1))
    `(progn ,@(mapcar (lambda (func) `(add-hook ',(eval hook) ',func)) (eval funcs))))

  (defmacro add-to-hooks (func hooks)
    "Add one FUNC to a series of hooks. HOOKS can be a list."
    (declare (indent 1))
    `(progn ,@(mapcar (lambda (hook) `(add-hook ',hook ',(eval func))) (eval hooks))))

  (defmacro add-hook! (hook &rest body)
    "A shortcut macro for `add-hook' that auto-wraps `body' in a lambda"
    (declare (indent 1))
    `(add-hook ,hook (lambda() ,@body)))

  ;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;
  (after "evil"
    (defmacro excmd (command func)
      "An alternative to `evil-ex-define-cmd' that won't choke on autoload
functions being registered as ex commands."
      `(evil-ex-define-cmd ,command ,func))

    (defmacro excmd! (command func)
      `(evil-ex-define-cmd ,command (lambda () (interactive) (call-interactively ,func))))

    (defmacro bind (&rest keys)
      "A minimalistic and evil-centric way of binding keys. KEYS is
made up of either:

1. Any of the following keywords:

:if CONDITION           Determines where these keymaps should be set.
:prefix PREFIX          Key(s) to prefix keymappings with
:map KEYMAP             Keymaps to bind keys to. Can be a list.
:global                 Tags these keymaps for the global keymap
:local                  Ditto, but for local keymap
:leader                 Like :prefix ?,
:localleader            Like :prefix ?\\
:<evil state>           e.g. :normal ;visual and so on. Anything that
                        `evil-state-p' recognizes You can stack to
                        make a key present in more than one state.

2. A key (as a vector e.g. [escape], a string \"<escape>\", or
character ?\^?).

3. A key definition: a symbol or a lambda function. "
      (declare (indent 9999))
      (let* ((condition t)
             (default-keymaps '(narf-mode-map))
             (keymaps default-keymaps)
             (forms '())
             (consecutive t)
             (states '())
             prefix local-p
             key def)
        (while keys
          (setq key (pop keys))
          (cond ((or (evil-state-p key)
                     (and (listp key)
                          (--all? (evil-state-p it) key)))
                 (setq states (-list key)))

                ((keywordp key)
                 (cl-case key
                   (:prefix
                    (let ((val (pop keys)))
                      (cond ((or (stringp val)
                                 (characterp val)
                                 (vectorp val))
                             (setq prefix val))
                            ((eq val 'leader)
                             (setq prefix narf/leader-key))
                            ((eq val 'localleader)
                             (setq prefix narf/localleader-key))
                            (t (signal 'bind-invalid-prefix prefix)))))
                   (:map
                    (let ((val (pop keys)))
                      (cond ((or (symbolp val) (listp val))
                             (setq keymaps (-list val)))
                            ((null val)
                             (setq keymaps default-keymaps))
                            (t (signal 'bind-invalid-keymaps '(key val))))))
                   (:if     (setq condition (pop keys)))
                   (:local  (setq local-p (pop keys)))
                   ;; TODO: Deprecated
                   (otherwise
                    (let ((keyword (intern-soft (substring (symbol-name key) 1)))
                          (next-key (cadr keys)))
                      (when (evil-state-p keyword)
                        (setq local-p nil)
                        (if consecutive
                            (cl-pushnew keyword states)
                          (setq states (list keyword))))
                      (setq consecutive t)))))

                ;; key-definition pairs
                ((or (stringp key)
                     (vectorp key)
                     (characterp key))
                 (unless keys (signal 'bind-no-definition key))
                 (setq def (pop keys))
                 (when condition
                   ;; Process the key
                   (cond ((stringp key)    (setq key (kbd key)))
                         ((characterp key) (setq key (string key))))
                   (when prefix
                     (setq key (if (vectorp key)
                                   (vconcat prefix key)
                                 (concat (kbd prefix) key))))
                   ;; Do the binding
                   (if (null states)
                       (dolist (keymap keymaps)
                         (add-to-list 'forms `(define-key ,keymap ,key ,def) t))
                     (dolist (state (-list states))
                       (when local-p
                         (setq keymaps (list (make-symbol (format "evil-%s-state-local-mode-map" state)))))
                       (dolist (keymap keymaps)
                         (add-to-list 'forms `(define-key ,(if local-p keymap `(evil-get-auxiliary-keymap ,keymap ',state t)) ,key ,def) t)))))
                 (setq consecutive nil))

                ;; fallback
                (t (signal 'bind-invalid-key key))))
        (when DEBUG-MODE (message "%s" forms))
        `(progn ,@forms)))))

(after "evil"
  (evil-define-command narf:exit-mode-maybe ()
    "Exits insert mode using jk without the momentary pause caused by
key-chord-define."
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (call-interactively 'self-insert-command)
      (let ((evt (read-event nil nil 0.4)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?k))

          (if (evil-replace-state-p)
              (evil-replace-backspace)
            (delete-char -1))
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t
          (setq unread-command-events (append unread-command-events (list evt)))))))))

;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun narf|enable-comment-hard-wrap ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill))

(defun narf|enable-hard-wrap ()
  (turn-on-auto-fill))

(defun narf|enable-tab-width-2 ()
  (setq tab-width 2 evil-shift-width 2))

(defun narf|enable-tab-width-4 ()
  (setq tab-width 4 evil-shift-width 4))

(defun narf|disable-final-newline ()
  (set (make-local-variable 'require-final-newline) nil))

(defun narf|enable-tabs ()
  (setq indent-tabs-mode t))

(defun narf|disable-tabs ()
  (setq indent-tabs-mode nil))

(defun narf|disable-delete-trailing-whitespace ()
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))


;;;; Global Defuns ;;;;;;;;;;;;;;;;;;;;;
(defun narf/minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (let (message-log-max)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark t)
      (when (get-buffer "*Completions*")
        (delete-windows-on "*Completions*"))
      (abort-recursive-edit))))


;;;; Project defuns ;;;;;;;;;;;;;;;;;;;;
(defvar narf/project-root-files '(".git" ".hg" ".svn" ".project" "local.properties" "project.properties" "rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs")
  "A list of files that count as 'project files', which determine whether a
    folder is the root of a project or not.")
(defun narf/project-root (&optional strict-p)
  "Get the path to the root of your project. Uses `narf/project-root-files' to
determine if a directory is a project."
  (let ((home (file-truename "~")))
    (catch 'found
      (f-traverse-upwards
       (lambda (path)
         (let ((path (file-truename path)))
           (if (file-equal-p home path)
               (throw 'found (if strict-p nil default-directory))
             (dolist (file narf/project-root-files)
               (when (file-exists-p (expand-file-name file path))
                 (throw 'found path)))))) default-directory)
    default-directory)))

(defun narf/project-has-files (files &optional root)
  "Return non-nil if `file' exists in the project root."
  (let ((root (or root (narf/project-root)))
        (files (if (listp files) files (list files)))
        found-p file)
    (while (and files (not found-p))
      (setq file (pop files))
      (setq found-p (file-exists-p (narf/project-path-to file root))))
    found-p))

(defun narf/project-path-to (file &optional root)
  (let ((root (or root (narf/project-root))))
    (expand-file-name file root)))

(defun narf/project-name (&optional root)
  (file-name-nondirectory (directory-file-name (or root (narf/project-root)))))

(defun narf/project-p ()
  (not (null (narf/project-root t))))

;; Make sure scratch buffer is always "in a project"
(defvar narf--project-scratch-buffer nil)
(defun narf/project-create-scratch-buffer ()
  (let* ((scratch-buffer (get-buffer-create "*scratch*"))
         (root (narf/project-root t))
         (project-name (narf/project-name root))
         (current-buffer (current-buffer)))
    (when root
      (mapc (lambda (b)
              (when (and (string-match-p "\\*scratch\\* (.+)" (buffer-name b))
                         (not (eq current-buffer b))
                         (= (buffer-size b) 0))
                (kill-buffer b)))
            (buffer-list))
      (save-window-excursion
        (switch-to-buffer scratch-buffer)
        (setq narf--project-scratch-buffer scratch-buffer)
        (erase-buffer)
        (cd root)
        (rename-buffer (format "*scratch* (%s)" project-name))))))
(add-hook 'find-file-hook 'narf/project-create-scratch-buffer)


(provide 'defuns)
;; defuns.el ends here
