;;; tools/eval/autoload/settings.el -*- lexical-binding: t; -*-

;;
;; REPLs

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl-other-window' and filled with the `:repl' setting.")

;;;###autodef
(defun set-repl-handler! (modes command &rest plist)
  "Defines a REPL for MODES.

MODES is either a single major mode symbol or a list of them. COMMAND is a
function that creates and returns the REPL buffer.

COMMAND can either be a function that takes no arguments, or an interactive
command that will be called interactively. COMMANDS must return either the repl
buffer or a function that takes no arguments and returns the repl buffer.

PLIST is a property list that map special attributes to this repl. These are
recognized:

  :persist BOOL
    If non-nil, this REPL won't be killed when its window is closed."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (setf (alist-get mode +eval-repls)
          (cons command plist))))


;;
;; Evaluation

;;;###autoload
(defvar +eval-runners nil
  "Alist mapping major modes to interactive runner functions.")

;;;###autodef
(defun set-eval-handler! (modes command)
  "Define a code evaluator for major mode MODES with `quickrun'.

MODES can be list of major mode symbols, or a single one.

1. If MODE is a string and COMMAND is the string, MODE is a file regexp and
   COMMAND is a string key for an entry in `quickrun-file-alist'.
2. If MODE is not a string and COMMAND is a string, MODE is a major-mode symbol
   and COMMAND is a key (for `quickrun--language-alist'), and will be registered
   in `quickrun--major-mode-alist'.
3. If MODE is not a string and COMMAND is an alist, see `quickrun-add-command':
   (quickrun-add-command MODE COMMAND :mode MODE).
4. If MODE is not a string and COMMANd is a symbol, add it to
   `+eval-runners', which is used by `+eval/region'."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
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
               (or (cdr (assq mode quickrun--major-mode-alist))
                   (string-remove-suffix "-mode" (symbol-name mode)))
               command :mode mode))))))
