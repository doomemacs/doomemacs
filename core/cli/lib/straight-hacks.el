;;; core/cli/straight-hacks.el --- -*- lexical-binding: t; no-byte-compile: t; -*-

;; Straight was designed primarily for interactive use, in an interactive Emacs
;; session, but Doom does its package management in the terminal. Some things
;; must be modified get straight to behave and improve its UX for our users.

(defvar doom--straight-auto-options
  '(("has diverged from"
     . "^Reset [^ ]+ to branch")
    ("but recipe specifies a URL of"
     . "Delete remote \"[^\"]+\", re-create it with correct URL")
    ("has a merge conflict:"
     . "^Abort merge$")
    ("has a dirty worktree:"
     . "^Discard changes$")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"main\") is ahead of default branch \"master\""
     . "^Checkout branch \"master\"")
    ("^In repository \"[^\"]+\", [^ ]+ (on branch \"[^\"]+\") is ahead of default branch \"[^\"]+\""
     . "^Checkout branch \"")
    ("^In repository "
     . "^Reset branch \\|^Delete remote [^,]+, re-create it with correct URL"))
  "A list of regexps, mapped to regexps.

Their CAR is tested against the prompt, and CDR is tested against the presented
option, and is used by `straight-vc-git--popup-raw' to select which option to
recommend.

It may not be obvious to users what they should do for some straight prompts,
so Doom will recommend the one that reverts a package back to its (or target)
original state.")


;; HACK Remove dired & magit options from prompt, since they're inaccessible in
;;      noninteractive sessions.
(advice-add #'straight-vc-git--popup-raw :override #'straight--popup-raw)

;; HACK Replace GUI popup prompts (which hang indefinitely in tty Emacs) with
;;      simple prompts.
(defadvice! doom--straight-fallback-to-y-or-n-prompt-a (orig-fn &optional prompt)
  :around #'straight-are-you-sure
  (or doom-auto-accept
      (if doom-interactive-p
          (funcall orig-fn prompt)
        (y-or-n-p (format! "%s" (or prompt ""))))))

(defun doom--straight-recommended-option-p (prompt option)
  (cl-loop for (prompt-re . opt-re) in doom--straight-auto-options
           if (string-match-p prompt-re prompt)
           return (string-match-p opt-re option)))

(defadvice! doom--straight-fallback-to-tty-prompt-a (orig-fn prompt actions)
  "Modifies straight to prompt on the terminal when in noninteractive sessions."
  :around #'straight--popup-raw
  (if doom-interactive-p
      (funcall orig-fn prompt actions)
    (let ((doom--straight-auto-options doom--straight-auto-options))
      ;; We can't intercept C-g, so no point displaying any options for this key
      ;; when C-c is the proper way to abort batch Emacs.
      (delq! "C-g" actions 'assoc)
      ;; HACK These are associated with opening dired or magit, which isn't
      ;;      possible in tty Emacs, so...
      (delq! "e" actions 'assoc)
      (delq! "g" actions 'assoc)
      (if doom-auto-discard
          (cl-loop with doom-auto-accept = t
                   for (_key desc func) in actions
                   when desc
                   when (doom--straight-recommended-option-p prompt desc)
                   return (funcall func))
        (print! (start "%s") (red prompt))
        (print-group!
         (terpri)
         (let (recommended options)
           (print-group!
            (print! " 1) Abort")
            (cl-loop for (_key desc func) in actions
                     when desc
                     do (push func options)
                     and do
                     (print! "%2s) %s" (1+ (length options))
                             (if (doom--straight-recommended-option-p prompt desc)
                                 (progn
                                   (setq doom--straight-auto-options nil
                                         recommended (length options))
                                   (green (concat desc " (Choose this if unsure)")))
                               desc))))
           (terpri)
           (let* ((options
                   (cons (lambda ()
                           (let ((doom-output-indent 0))
                             (terpri)
                             (print! (warn "Aborted")))
                           (kill-emacs 1))
                         (nreverse options)))
                  (prompt
                   (format! "How to proceed? (%s%s) "
                            (mapconcat #'number-to-string
                                       (number-sequence 1 (length options))
                                       ", ")
                            (if (not recommended) ""
                              (format "; don't know? Pick %d" (1+ recommended)))))
                  answer fn)
             (while (null (nth (setq answer (1- (read-number prompt)))
                               options))
               (print! (warn "%s is not a valid answer, try again.")
                       answer))
             (funcall (nth answer options)))))))))

(setq straight-arrow " > ")
(defadvice! doom--straight-respect-print-indent-a (string &rest objects)
  "Same as `message' (which see for STRING and OBJECTS) normally.
However, in batch mode, print to stdout instead of stderr."
  :override #'straight--output
  (let ((msg (apply #'format string objects)))
    (save-match-data
      (when (string-match (format "^%s\\(.+\\)$" (regexp-quote straight-arrow)) msg)
        (setq msg (match-string 1 msg))))
    (and (string-match-p "^\\(Cloning\\|\\(Reb\\|B\\)uilding\\) " msg)
         (not (string-suffix-p "...done" msg))
         (doom--print (doom--format (concat "> " msg))))))
