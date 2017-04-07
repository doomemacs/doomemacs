;;; feature/eval/config.el

;; This module creates a centralized way of invoking/sending selections to REPLs
;; (with `repl-toggle'), building projects/files, and running code (with
;; `quickrun'), revealing its output in a popup window.

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
  `(dolist (mode ',(if (listp modes) modes (list modes)) +eval-builders)
     (unless (assq mode +eval-builders)
       (push (list mode) +eval-builders))
     (push (cons ',name (append (list :fn #',fn) ',plist))
           (cdr (assq mode +eval-builders)))))


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
  "Define a REPL for a mode. MODE is a major mode and COMMAND is a function that
invokes the repl. Takes the same arguements as `rtog/add-repl'."
  `(push ',(cons mode command) +eval-repls))

(set! :popup
  '(:custom (lambda (b &rest _) (buffer-local-value '+eval-repl-mode b)))
  :size 16 :noesc t)


;;
;; Evaluation
;;

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(def-setting! :eval (mode command)
  "Define a code evaluator for major mode MODE with `quickrun'.

1. If MODE is a string and COMMAND is the string, MODE is a file regexp and
   COMMAND is a string key for an entry in `quickrun-file-alist'.
2. If MODE is not a string and COMMAND is a string, MODE is a major-mode symbol
   and COMMAND is a key (for `quickrun--language-alist'), and will be registered
   in `quickrun--major-mode-alist'.
3. If MODE is not a string and COMMAND is an alist, see `quickrun-add-command':
   (quickrun-add-command MODE COMMAND :mode MODE)."
  `(after! quickrun
     ,(cond ((stringp command)
             `(push ',(cons mode command)
                    ,(if (stringp mode)
                         'quickrun-file-alist
                       'quickrun--major-mode-alist)))
            ((listp command)
             `(quickrun-add-command
                ,(symbol-name mode)
                ',command :mode ',mode)))))

(def-package! quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :init
  ;; Use standard linum-mode for quickrun eval windows; so we can have different
  ;; rules for it.
  (add-hook 'quickrun/mode-hook 'linum-mode)

  :config
  (set! :popup "*quickrun*" :size 10)

  ;; don't auto-focus quickrun windows. Shackle handles that for us.
  (setq quickrun-focus-p nil)

  (defun +eval*quickrun-auto-close (&rest _)
    "Allows us to silently re-run quickrun from within the quickrun buffer."
    (when-let (win (get-buffer-window quickrun--buffer-name))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))
  (advice-add 'quickrun :before '+eval*quickrun-auto-close)
  (advice-add 'quickrun-region :before '+eval*quickrun-auto-close)

  (defun +eval|quickrun-scroll-to-bof ()
    "Ensures window is scrolled to BOF on invocation."
    (with-selected-window (get-buffer-window quickrun--buffer-name)
      (goto-char (point-min))))
  (add-hook 'quickrun-after-run-hook '+eval|quickrun-scroll-to-bof))

