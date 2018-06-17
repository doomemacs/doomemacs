;;; feature/eval/autoload/settings.el -*- lexical-binding: t; -*-

;;
;; REPLs
;;

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl' and filled with the `:repl' setting.")

;;;###autodef
(defun set-repl-handler! (mode command)
  "Define a REPL for a mode. MODE is a major mode symbol and COMMAND is a
function that creates and returns the REPL buffer."
  (push (cons mode command) +eval-repls))

;; FIXME obsolete :repl
;;;###autoload
(def-setting! :repl (mode command)
  :obsolete set-repl-handler!
  `(push (cons ,mode ,command) +eval-repls))


;;
;; Evaluation
;;


(defvar +eval-runners nil
  "Alist mapping major modes to interactive runner functions.")

;;;###autodef
(defun set-eval-handler! (mode command)
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
  (cond ((symbolp command)
         (push (cons mode command) +eval-runners))
        ((stringp command)
         (after! quickrun
           (push (cons mode command)
                 (if (stringp mode)
                     quickrun-file-alist
                   quickrun--major-mode-alist))))
        ((listp command)
         (after! quickrun
           (quickrun-add-command
             (symbol-name mode)
             command :mode mode)))))

;; FIXME obsolete :eval
;;;###autoload
(def-setting! :eval (mode command)
  :obsolete set-eval-handler!
  `(set-eval-handler! ,mode ,command))
