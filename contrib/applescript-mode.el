;;; applescript-mode.el --- major mode for editing AppleScript source

;; Copyright (C) 2004  MacEmacs JP Project

;;; Credits:
;; Copyright (C) 2003,2004 FUJIMOTO Hisakuni
;;   http://www.fobj.com/~hisa/w/applescript.el.html
;; Copyright (C) 2003 443,435 (Public Domain?)
;;   http://pc.2ch.net/test/read.cgi/mac/1034581863/
;; Copyright (C) 2004 Harley Gorrell <harley@mahalito.net>
;;   http://www.mahalito.net/~harley/elisp/osx-osascript.el

;; Author: sakito <sakito@users.sourceforge.jp>
;; Keywords: languages, tools

(defconst applescript-mode-version "$Revision$"
  "The current version of the AppleScript mode.")

(defconst applescript-mode-help-address "sakito@users.sourceforge.jp"
  "Address accepting submission of bug reports.")

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; acquired from http://github.com/ieure/applescript-mode/

;;; Commentary:

;; AppleScript Mode

;;; Usage:
;; To use applescript-mode.el put the following line in your .emacs:
;; (autoload 'applescript-mode "applescript-mode"
;;   "Major mode for editing AppleScript source." t)
;; (add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))

;; Please use the SourceForge MacEmacs JP Project to submit bugs or
;; patches:
;;
;;     http://sourceforge.jp/projects/macemacsjp

;; FOR MORE INFORMATION:

;; There is some information on applescript-mode.el at

;;     http://macemacsjp.sourceforge.jp/documents/applescript-mode/


;;; Code:



;; user customize variables
(defgroup applescript nil
  "Support for AppleScript, <http://www.apple.com/applescript/>"
  :group 'languages
  :prefix "as-")

(defcustom as-osascript-command "osascript"
  "*execute AppleScripts and other OSA language scripts."
  :type 'string
  :group 'applescript)

(defcustom as-osacompile-command "osacompile"
  "*compile AppleScripts and other OSA language scripts."
  :type 'string
  :group 'applescript)

(defcustom as-osascript-command-args '("-ss")
  "*List of string arguments to be used when starting a osascript."
  :type '(repeat string)
  :group 'applescript)

(defcustom as-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[as-guess-indent-offset]' can usually guess a good value when
you're editing someone else's AppleScript code."
  :type 'integer
  :group 'applescript)

(defcustom as-continuation-offset 4
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a
Continuation sign terminated line.  Only those continuation lines
for a block opening statement are given this extra offset."
  :type 'integer
  :group 'applescript)

;; Face Setting

;; Face for true, false, me, it
(defvar as-pseudo-keyword-face 'as-pseudo-keyword-face
  "Face for pseudo keywords in AppleScript mode, like me, true, false.")
(make-face 'as-pseudo-keyword-face)

;; Face for command
(defvar as-command-face 'as-command-face
  "Face for command like copy, get, set, and error.")
(make-face 'as-command-face)

(defun as-font-lock-mode-hook ()
  (or (face-differs-from-default-p 'as-pseudo-keyword-face)
      (copy-face 'font-lock-keyword-face 'as-pseudo-keyword-face))
  (or (face-differs-from-default-p 'as-command-face)
      (copy-face 'font-lock-keyword-face 'as-command-face)))
(add-hook 'font-lock-mode-hook 'as-font-lock-mode-hook)

(defvar applescript-font-lock-keywords
  (let (
        ;; expressions and control Statements
        (kw1 (regexp-opt
              '("and" "app" "application" "considering" "div"
                "else" "end" "exit" "is" "mod" "not" "on" "or"
                "if" "ignoring" "reopen" "repeat"
                "tell" "then" "to"
                "using[ \t]terms[ \t]from"
                "with[ \t]timeout" "with[ \t]transaction")))
        ;; commands
        (kw2 (regexp-opt
              '("ASCII[ \t]character" "ASCII[ \t]number" "activate" "AGStart"
                "beep"  "copy" "count" "choose[ \t]application"
                "choose[ \t]file" "choose[ \t]folder" "close[ \t]access"
                "current[ \t]date" "display[ \t]dialog" "get" "get[ \t]EOF"
                "info[ \t]for" "launch" "list[ \t]disks" "list[ \t]folder"
                "load[ \t]script" "log" "monitor[ \t]depth" "max[ \t]monitor[ \t]depth"
                "min[ \t]monitor[ \t]depth" "new[ \t]file" "offset"
                "open[ \t]for[ \t]access" "path[ \t]to" "random[ \t]number"
                "read" "round" "run" "run[ \t]script" "scripting[ \t]component"
                "set" "set[ \t]EOF" "set[ \t]monitor[ \t]depth" "set[ \t]volume"
                "start[ \t]log" "stop[ \t]log" "store[ \t]script"
                "time[ \t]to[ \t]GMT" "write")))
        ;; misc
        (kw3 (regexp-opt
              '("buttons" "default[ \t]answer" "default[ \t]button"
                "to[ \t]begining[ \t]of" "to[ \t]word" "starting[ \t]at"
                "with[ \t]icon" "write[ \t]permission"))))
    (list
     ;; keywords
     (cons (concat "\\b\\(" kw1 "\\)\\b[ \n\t(]") 1)
     (cons (concat "\\b\\(" kw2 "\\)\\b[ \n\t(]") 1)
     ;; kw3 not yet..
     (cons (concat "\\b\\(" kw3 "\\)\\b[ \n\t(]") 1)
     ;; functions
     '("\\bon[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     '("\\bto[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ;; pseudo-keywords
     '("\\b\\(it\\|me\\|my\\|true\\|false\\)\\b"
       1 as-pseudo-keyword-face))))
(put 'applescript-mode 'font-lock-defaults '(applescript-font-lock-keywords))

;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar applescript-mode-abbrev-table nil
  "Abbrev table in use in `applescript-mode' buffers.")
(define-abbrev-table 'applescript-mode-abbrev-table nil)

(defvar applescript-mode-hook nil
  "*Hook called by `applescript-mode'.")

(defvar as-mode-map ()
  "Keymap used in `applescript-mode' buffers.")
(if as-mode-map
    nil
  (setq as-mode-map (make-sparse-keymap))
  ;; Key bindings

  ;; subprocess commands
  (define-key as-mode-map "\C-c\C-c" 'as-execute-buffer)
  (define-key as-mode-map "\C-c\C-s" 'as-execute-string)
  (define-key as-mode-map "\C-c|" 'as-execute-region)

  ;; Miscellaneous
  (define-key as-mode-map "\C-c;" 'comment-region)
  (define-key as-mode-map "\C-c:" 'uncomment-region))

(defvar as-mode-syntax-table nil
  "Syntax table used in `applescript-mode' buffers.")
(when (not as-mode-syntax-table)
  (setq as-mode-syntax-table (make-syntax-table))

  (modify-syntax-entry ?\" "\"" as-mode-syntax-table)

  (modify-syntax-entry ?:  "." as-mode-syntax-table)
  (modify-syntax-entry ?\; "." as-mode-syntax-table)
  (modify-syntax-entry ?&  "." as-mode-syntax-table)
  (modify-syntax-entry ?\|  "." as-mode-syntax-table)
  (modify-syntax-entry ?+  "." as-mode-syntax-table)
  (modify-syntax-entry ?/  "." as-mode-syntax-table)
  (modify-syntax-entry ?=  "." as-mode-syntax-table)
  (modify-syntax-entry ?<  "." as-mode-syntax-table)
  (modify-syntax-entry ?>  "." as-mode-syntax-table)
  (modify-syntax-entry ?$ "." as-mode-syntax-table)
  (modify-syntax-entry ?\[ "." as-mode-syntax-table)
  (modify-syntax-entry ?\] "." as-mode-syntax-table)
  (modify-syntax-entry ?\{ "." as-mode-syntax-table)
  (modify-syntax-entry ?\} "." as-mode-syntax-table)
  (modify-syntax-entry ?. "." as-mode-syntax-table)
  (modify-syntax-entry ?\\ "." as-mode-syntax-table)
  (modify-syntax-entry ?\' "." as-mode-syntax-table)

  ;; a double hyphen starts a comment
  (modify-syntax-entry ?-  ". 12" as-mode-syntax-table)

  ;; comment delimiters
  (modify-syntax-entry ?\f  ">   " as-mode-syntax-table)
  (modify-syntax-entry ?\n  ">   " as-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()1" as-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" as-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23b" as-mode-syntax-table))

;; Utilities
(defmacro as-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  (` (condition-case nil
         (progn (,@ body))
       (error nil))))

(defsubst as-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (when (featurep 'xemacs)
    (and (boundp 'zmacs-region-stays)
         (setq zmacs-region-stays t))))

(defsubst as-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of on or to
  eod  -- end of on or to
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (as-beginning-of-handler 'either))
     ((eq position 'eod) (as-end-of-handler 'either))
     ;; Kind of funny, I know, but useful for as-up-exception.
     ((eq position 'bob) (beginning-of-buffer))
     ((eq position 'eob) (end-of-buffer))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (as-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position)))
    (prog1 (point) (goto-char here))))

;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
(defvar as-menu nil
  "Menu for AppleScript Mode.
This menu will get created automatically if you have the
`easymenu' package.  Note that the latest X/Emacs releases
contain this package.")

(and (as-safe (require 'easymenu) t)
     (easy-menu-define
      as-menu as-mode-map "AppleScript Mode menu"
      '("AppleScript"
        ["Comment Out Region"   comment-region (mark)]
        ["Uncomment Region"     uncomment-region (mark)]
        "-"
        ["Execute buffer"       as-execute-buffer t]
        ["Execute region"       as-execute-region (mark)]
        ["Execute string"       as-execute-string t]
        "-"
        ["Mode Version"         as-mode-version t]
        ["AppleScript Version"   as-language-version t])))

;;;###autoload
(defun applescript-mode ()
  "Major mode for editing AppleScript files."
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  (make-local-variable 'fill-paragraph-function)
  ;;

  ;; Maps and tables
  (use-local-map as-mode-map)

  (set-syntax-table as-mode-syntax-table)

  ;; Names
  (setq major-mode 'applescript-mode
        mode-name "AppleScript"
        local-abbrev-table applescript-mode-abbrev-table
        font-lock-defaults '(applescript-font-lock-keywords)
        paragraph-separate "[ \t\n\f]*$"
        paragraph-start    "[ \t\n\f]*$"
        require-final-newline t
        comment-start "-- "
        comment-end   ""
        comment-start-skip "---*[ \t]*"
        comment-column 40)

  ;;  Support for outline-minor-mode
  (set (make-local-variable 'outline-regexp)
       "\\([ \t]*\\(on\\|to\\|if\\|repeat\\|tell\\|end\\)\\|--\\)")
  (set (make-local-variable 'outline-level) 'as-outline-level)

  ;; add the menu
  (if as-menu
      (easy-menu-add as-menu))

  ;; Run the mode hook.  Note that applescript-mode-hook is deprecated.
  (if applescript-mode-hook
      (run-hooks 'applescript-mode-hook)
    (run-hooks 'applescript-mode-hook)))

(when (not (or (rassq 'applescript-mode auto-mode-alist)
  (push '("\\.applescript$" . applescript-mode) auto-mode-alist))))

;;; Subprocess commands

;; function use variables
(defconst as-output-buffer "*AppleScript Output*"
  "Name for the buffer to display AppleScript output.")
(make-variable-buffer-local 'as-output-buffer)

;; Code execution commands
(defun as-execute-buffer (&optional async)
  "Execute the whole buffer as an Applescript"
  (interactive "P")
  (as-execute-region (point-min) (point-max) async))

(defun as-execute-string (string &optional async)
  "Send the argument STRING to an AppleScript."
  (interactive "sExecute AppleScript: ")
  (save-excursion
    (set-buffer (get-buffer-create
                 (generate-new-buffer-name as-output-buffer)))
    (insert string)
    (as-execute-region (point-min) (point-max) async)))

(defun as-execute-region (start end &optional async)
  "Execute the region as an Applescript"
  (interactive "r\nP")
  (let ((region (buffer-substring start end))
        (as-current-win (selected-window)))
    (pop-to-buffer as-output-buffer)
    (insert (as-execute-code region))
    (select-window as-current-win)))

(defun as-execute-code (code)
  "pop to the AppleScript buffer, run the code and display the results."
  (as-decode-string
   (do-applescript (as-string-to-sjis-string-with-escape code))))

(defun as-mode-version ()
  "Echo the current version of `applescript-mode' in the minibuffer."
  (interactive)
  (message "Using `applescript-mode' version %s" applescript-mode-version)
  (as-keep-region-active))

(defun as-language-version()
  "Echo the current version of AppleScript Version in the minibuffer."
  (interactive)
  (message "Using AppleScript version %s"
           (as-execute-code "AppleScript's version"))
  (as-keep-region-active))

;; as-beginning-of-handler, as-end-of-handler,as-goto-initial-line not yet

;;  Support for outline.el
(defun as-outline-level ()
  "This is so that `current-column` DTRT in otherwise-hidden text"
  (let (buffer-invisibility-spec)
    (save-excursion
      (back-to-indentation)
      (current-column))))

;; for Japanese
(defun as-unescape-string (str)
  "delete escape char '\'"
  (replace-regexp-in-string "\\\\\\(.\\)" "\\1" str))

(defun as-escape-string (str)
  "add escape char '\'"
  (replace-regexp-in-string "\\\\" "\\\\\\\\" str))

(defun as-sjis-byte-list-escape (lst)
  (cond
   ((null lst) nil)
   ((= (car lst) 92)
    (append (list 92 (car lst)) (as-sjis-byte-list-escape (cdr lst))))
   (t (cons (car lst) (as-sjis-byte-list-escape (cdr lst))))))

(defun as-string-to-sjis-string-with-escape (str)
  "String convert to SJIS, and escape \"\\\" "
  (apply 'string
         (as-sjis-byte-list-escape
          (string-to-list
           (as-encode-string str)))))

(defun as-decode-string (str)
  "do-applescript return values decode sjis-mac"
  (decode-coding-string str 'sjis-mac))

(defun as-encode-string (str)
  "do-applescript return values encode sjis-mac"
  (encode-coding-string str 'sjis-mac))

(defun as-parse-result (retstr)
  "String convert to Emacs Lisp Object"
  (cond
   ;; as list
   ((string-match "\\`\\s-*{\\(.*\\)}\\s-*\\'" retstr)
    (let ((mstr (match-string 1 retstr)))
      (mapcar (lambda (item) (as-parse-result item))
              (split-string mstr ","))))

   ;; AERecord item as cons
   ((string-match "\\`\\s-*\\([^:\\\"]+\\):\\(.*\\)\\s-*\\'" retstr)
    (cons (intern (match-string 1 retstr))
          (as-parse-result (match-string 2 retstr))))

   ;; as string
   ((string-match "\\`\\s-*\\\"\\(.*\\)\\\"\\s-*\\'" retstr)
    (as-decode-string
     (as-unescape-string (match-string 1 retstr))))

   ;; as integer
   ((string-match "\\`\\s-*\\([0-9]+\\)\\s-*\\'" retstr)
    (string-to-int (match-string 1 retstr)))

    ;; else
    (t (intern retstr))))

(provide 'applescript-mode)
;;; applescript-mode.el ends here
