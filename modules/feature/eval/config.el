;;; feature/eval/config.el -*- lexical-binding: t; -*-

;;
;; Code building
;;

(defvar +eval-builders nil
  "A nested alist, mapping major modes to build function plists. Used by
`+eval/build' and filled with the `:build' setting.")

(def-setting! :build (name modes fn &rest plist)
  "Define a build function (FN) for MODES (can be major or minor) called NAME.

PLIST accepts the following properties:

  :when FORM    A predicate to determine if the builder is appropriate for this
                buffer."
  `(dolist (mode ',(doom-enlist (doom-unquote modes)) +eval-builders)
     (unless (assq mode +eval-builders)
       (push (list mode) +eval-builders))
     (cl-pushnew (cons ,name (append (list :fn ,fn) (list ,@plist)))
                 (cdr (assq mode +eval-builders))
                 :test #'eq :key #'car)))


;;
;; REPLs
;;

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/repl' and filled with the `:repl' setting.")

(define-minor-mode +eval-repl-mode
  "A minor mode for REPL buffers."
  :init-value nil)

(def-setting! :repl (mode command)
  "Define a REPL for a mode. MODE is a major mode symbol and COMMAND is a
function that creates and returns the REPL buffer."
  `(push (cons ,mode ,command) +eval-repls))

(set! :popup
  '(:custom (lambda (b &rest _) (buffer-local-value '+eval-repl-mode b)))
  :size 16 :noesc t)


;;
;; Evaluation
;;

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(defvar +eval-runners-alist nil
  "Alist mapping major modes to interactive runner functions.")

(def-setting! :eval (mode command)
  "Define a code evaluator for major mode MODE with `quickrun'.

1. If MODE is a string and COMMAND is the string, MODE is a file regexp and
   COMMAND is a string key for an entry in `quickrun-file-alist'.
2. If MODE is not a string and COMMAND is a string, MODE is a major-mode symbol
   and COMMAND is a key (for `quickrun--language-alist'), and will be registered
   in `quickrun--major-mode-alist'.
3. If MODE is not a string and COMMAND is an alist, see `quickrun-add-command':
   (quickrun-add-command MODE COMMAND :mode MODE).
4. If MODE is not a string and COMMANd is a symbol, add it to
   `+eval-runners-alist', which is used by `+eval/region'."
  (let ((command (doom-unquote command)))
    (cond ((symbolp command)
           `(push (cons ,mode ',command) +eval-runners-alist))
          ((stringp command)
           `(after! quickrun
              (push (cons ,mode ',command)
                    ,(if (stringp mode)
                         'quickrun-file-alist
                       'quickrun--major-mode-alist))))
          ((listp command)
           `(after! quickrun
              (quickrun-add-command
                ,(symbol-name (doom-unquote mode))
                ',command :mode ,mode))))))

(def-package! quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :init
  (add-hook 'quickrun--mode-hook #'nlinum-mode)
  :config
  (set! :popup
    '("*quickrun*"       :size 10 :noesc t    :autokill t :autoclose t)
    '("*eval*"           :size 12 :noselect t :autokill t :autoclose t)
    '("*Pp Eval Output*" :size 12 :noselect t :autokill t :autoclose t))

  (defun +eval*quickrun-auto-close (&rest _)
    "Allows us to silently re-run quickrun from within the quickrun buffer."
    (when-let (win (get-buffer-window quickrun--buffer-name))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))
  (advice-add #'quickrun :before #'+eval*quickrun-auto-close)
  (advice-add #'quickrun-region :before #'+eval*quickrun-auto-close)

  (defun +eval|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (with-selected-window (get-buffer-window quickrun--buffer-name)
      (goto-char (point-min))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-scroll-to-bof))

