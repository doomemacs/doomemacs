;;; feature/eval/config.el -*- lexical-binding: t; -*-

;;
;; REPLs
;;

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl' and filled with the `:repl' setting.")

(define-minor-mode +eval-repl-mode
  "A minor mode for REPL buffers.")

(def-setting! :repl (mode command)
  "Define a REPL for a mode. MODE is a major mode symbol and COMMAND is a
function that creates and returns the REPL buffer."
  `(push (cons ,mode ,command) +eval-repls))


;;
;; Evaluation
;;

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(defvar +eval-runners nil
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
   `+eval-runners', which is used by `+eval/region'."
  (let ((command (doom-unquote command)))
    (cond ((symbolp command)
           `(push (cons ,mode ',command) +eval-runners))
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
  (unless (boundp 'display-line-numbers)
    (add-hook 'quickrun--mode-hook #'nlinum-mode))
  :config
  (setq quickrun-focus-p nil)

  (set! :popup "^\\*quickrun" '((size . 0.3)) '((transient . 0)))

  (defun +eval*quickrun-auto-close (&rest _)
    "Allows us to silently re-run quickrun from within the quickrun buffer."
    (when-let* ((win (get-buffer-window quickrun--buffer-name)))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))
  (advice-add #'quickrun :before #'+eval*quickrun-auto-close)
  (advice-add #'quickrun-region :before #'+eval*quickrun-auto-close)

  (defun +eval|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (with-selected-window (get-buffer-window quickrun--buffer-name)
      (goto-char (point-min))
      (let ((ignore-window-parameters t))
        (shrink-window-if-larger-than-buffer))))
  (add-hook 'quickrun-after-run-hook #'+eval|quickrun-scroll-to-bof))

