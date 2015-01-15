;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 1988-92, 1994-95, 1999, 2000, 2003, 2007, 2008
;;; Free Software Foundation, Inc.
;;; Written by Steve Byrne.
;;;
;;; This file is part of GNU Smalltalk.
;;;
;;; GNU Smalltalk is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 2, or (at your option) any later 
;;; version.
;;;
;;; GNU Smalltalk is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with GNU Smalltalk; see the file COPYING.  If not, write to the Free
;;; Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Incorporates Frank Caggiano's changes for Emacs 19.
;;; Updates and changes for Emacs 20 and 21 by David Forster

(require 'comint)

(defvar smalltalk-prompt-pattern "^st> *"
  "Regexp to match prompts in smalltalk buffer.")

(defvar *gst-process* nil
  "Holds the GNU Smalltalk process")
(defvar gst-program-name "/usr/local/Cellar/gnu-smalltalk/3.2.5_1/bin/gst -V"
  "GNU Smalltalk command to run.  Do not use the -a, -f or -- options.")

(defvar smalltalk-command-string nil
  "Non nil means that we're accumulating output from Smalltalk")

(defvar smalltalk-eval-data nil
  "?")

(defvar smalltalk-ctl-t-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-d" 'smalltalk-toggle-decl-tracing)
    (define-key keymap "\C-e" 'smalltalk-toggle-exec-tracing)
    (define-key keymap "\C-v" 'smalltalk-toggle-verbose-exec-tracing)
    keymap)
  "Keymap of subcommands of C-c C-t, tracing related commands")

(defvar gst-mode-map
  (let ((keymap (copy-keymap comint-mode-map)))
    (define-key keymap "\C-c\C-t" smalltalk-ctl-t-map)

    (define-key keymap "\C-\M-f"   'smalltalk-forward-sexp)
    (define-key keymap "\C-\M-b"   'smalltalk-backward-sexp)
    (define-key keymap "\C-cd" 'smalltalk-doit)
    (define-key keymap "\C-cf" 'smalltalk-filein)
    (define-key keymap "\C-cp" 'smalltalk-print)
    (define-key keymap "\C-cq" 'smalltalk-quit)
    (define-key keymap "\C-cs" 'smalltalk-snapshot)
    keymap)
  "Keymap used in Smalltalk interactor mode.")

(defun gst (command-line)
  "Invoke GNU Smalltalk"
  (interactive (list (if (null current-prefix-arg)
			 gst-program-name
		       (read-smalltalk-command))))
  (setq gst-program-name command-line)
  (funcall (if (not (eq major-mode 'gst-mode))
	       #'switch-to-buffer-other-window
	     ;; invoked from a Smalltalk interactor window, so stay
	     ;; there
	     #'identity)
	   (apply 'make-gst "gst" (parse-smalltalk-command gst-program-name)))
  (setq *smalltalk-process* (get-buffer-process (current-buffer))))

(defun read-smalltalk-command (&optional command-line)
  "Reads the program name and arguments to pass to Smalltalk,
providing COMMAND-LINE as a default (which itself defaults to
`gst-program-name'), answering the string."
  (read-string "Invoke Smalltalk: " (or command-line gst-program-name)))

(defun smalltalk-file-name (str)
  (if (file-name-directory str) (expand-file-name str) str))

(defun parse-smalltalk-command (&optional str)
  "Parse a list of command-line arguments from STR (default
`gst-program-name'), adding --emacs-mode and answering the list."
  (unless str (setq str gst-program-name))
  (let (start end result-args)
    (while (setq start (string-match "[^ \t]" str))
		(setq end (or (string-match " " str start) (length str)))
		(push (smalltalk-file-name (substring str start end)) result-args)
		(if (null (cdr result-args)) (push "--emacs-mode" result-args))
		(setq str (substring str end)))
    (nreverse result-args)))

(defun make-gst (name &rest switches)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc (setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      ;;    (setq size (buffer-size))
      (if (memq status '(run stop))
	  nil
	(if proc (delete-process proc))
	(setq proc (apply  'start-process
			   name buffer
			   "env"
			   ;; I'm choosing to leave these here
			   ;;"-"
			   (format "TERMCAP=emacs:co#%d:tc=unknown:"
				   (frame-width))
			   "TERM=emacs"
			   "EMACS=t"
			   switches))
	(setq name (process-name proc)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (set-process-filter proc 'gst-filter)
      (gst-mode))
    buffer))

(defun gst-filter (process string)
  "Make sure that the window continues to show the most recently output
text."
  (let (where ch command-str)
    (setq where 0)			;fake to get through the gate
    (while (and string where)
      (if smalltalk-command-string
	  (setq string (smalltalk-accum-command string)))
      (if (and string
	       (setq where (string-match "\C-a\\|\C-b" string)))
	  (progn
	    (setq ch (aref string where))
	    (cond ((= ch ?\C-a)		;strip these out
		   (setq string (concat (substring string 0 where)
					(substring string (1+ where)))))
		  ((= ch ?\C-b)		;start of command
		   (setq smalltalk-command-string "") ;start this off
		   (setq string (substring string (1+ where))))))))
    (save-excursion
      (set-buffer (process-buffer process))
      (goto-char (point-max))
      (and string
	   (setq mode-status "idle")
	   (insert string))
      (if (process-mark process)
	  (set-marker (process-mark process) (point-max)))))
  ;;  (if (eq (process-buffer process)
  ;;	  (current-buffer))
  ;;      (goto-char (point-max)))
					;  (save-excursion
					;      (set-buffer (process-buffer process))
					;      (goto-char (point-max))
  ;;      (set-window-point (get-buffer-window (current-buffer)) (point-max))
					;      (sit-for 0))
  (let ((buf (current-buffer)))
    (set-buffer (process-buffer process))
    (goto-char (point-max)) (sit-for 0)
    (set-window-point (get-buffer-window (current-buffer)) (point-max))
    (set-buffer buf)))

(defun smalltalk-accum-command (string)
  (let (where)
    (setq where (string-match "\C-a" string))
    (setq smalltalk-command-string
	  (concat smalltalk-command-string (substring string 0 where)))
    (if where
	(progn
	  (unwind-protect		;found the delimiter...do it
	      (smalltalk-handle-command smalltalk-command-string)
	    (setq smalltalk-command-string nil))
	  ;; return the remainder
	  (substring string where))
      ;; we ate it all and didn't do anything with it
      nil)))

(defun smalltalk-handle-command (str)
  (eval (read str)))

(defun gst-mode ()
  "Major mode for interacting Smalltalk subprocesses.

Entry to this mode calls the value of gst-mode-hook with no arguments,
if that value is non-nil; likewise with the value of comint-mode-hook.
gst-mode-hook is called after comint-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gst-mode)
  (setq mode-name "GST")
  (require 'comint)
  (comint-mode)
  (setq mode-line-format
	'("" mode-line-modified mode-line-buffer-identification "   "
	  global-mode-string "   %[(" mode-name ": " mode-status
	  "%n" mode-line-process ")%]----" (-3 . "%p") "-%-"))

  (setq comint-prompt-regexp smalltalk-prompt-pattern)
  (setq comint-use-prompt-regexp t) 
  (use-local-map gst-mode-map)
  (make-local-variable 'mode-status)
  (make-local-variable 'smalltalk-command-string)
  (setq smalltalk-command-string nil)
  (setq mode-status "starting-up")
  (run-hooks 'comint-mode-hook 'gst-mode-hook))


(defun smalltalk-print-region (start end &optional label)
  (let (str filename line pos extra)
    (save-excursion
      (save-restriction 
	(goto-char (max start end))
	(smalltalk-backward-whitespace)
	(setq pos (point))
	;canonicalize
	(while (progn (smalltalk-backward-whitespace)
		      (or (= (preceding-char) ?!)
		          (= (preceding-char) ?.)))
	    (backward-char 1))

	(setq str (buffer-substring (min start end) (point)))
	(setq extra (buffer-substring (point) pos))

	;; unrelated, but reusing save-excursion
	(goto-char (min start end))
	(setq pos (1- (point)))
	(setq filename (buffer-file-name))
	(widen)
	(setq line (1+ (count-lines 1 (point))))))
    (send-to-smalltalk (format "(%s) printNl%s\n" str extra)
		       (or label "eval")
		       (smalltalk-pos line pos))))

(defun smalltalk-eval-region (start end &optional label)
  "Evaluate START to END as a Smalltalk expression in Smalltalk window.
If the expression does not end with an exclamation point, one will be
added (at no charge)."
  (let (str filename line pos)
    (setq str (buffer-substring start end))
    (save-excursion
      (save-restriction 
	(goto-char (min start end))
	(setq pos (point))
	(setq filename (buffer-file-name))
	(widen)
	(setq line (1+ (count-lines 1 (point))))))
    (send-to-smalltalk (concat str "\n")
		       (or label "eval")
		       (smalltalk-pos line pos))))

(defun smalltalk-doit (use-line)
  (interactive "P")
  (let* ((start (or (mark) (point)))
	 (end (point))
	 (rgn (if (or use-line
		      (= start end))
		  (smalltalk-bound-expr)
		(cons start end))))
    (smalltalk-eval-region (car rgn) (cdr rgn) "doIt")))

(defun smalltalk-print (use-line)
  (interactive "P")
  (let* ((start (or (mark) (point)))
	 (end (point))
	 (rgn (if (or use-line
		      (= start end))
		  (smalltalk-bound-expr)
		(cons start end))))
    (smalltalk-print-region (car rgn) (cdr rgn) "printIt")))

(defun smalltalk-bound-expr ()
  "Returns a cons of the region of the buffer that contains a smalltalk expression."
  (save-excursion 
    (beginning-of-line) 
    (cons
     (point)
     (progn (next-line)
     	    (smalltalk-backward-whitespace)
	    (point)))))

(defun smalltalk-pos (line pos)
  (let ((filename (buffer-file-name)))
    (if filename (list line filename pos) nil)))

(defun smalltalk-compile (start end)
  (interactive "r")
  (let ((str (buffer-substring start end))
	(filename (buffer-file-name))
	(pos start)
	(line (save-excursion
		(save-restriction
		  (widen)
		  (setq line (1+ (line-number-at-pos start)))))))
    (send-to-smalltalk str "compile"
		       (smalltalk-pos line pos))))

(defun smalltalk-quote-strings (str)
  (let (new-str)
    (save-excursion
      (set-buffer (get-buffer-create " st-dummy "))
      (erase-buffer)
      (insert str)
      (goto-char 1)
      (while (and (not (eobp))
		  (search-forward "'" nil 'to-end))
	(insert "'"))
      (buffer-string))))

(defun smalltalk-snapshot (&optional snapshot-name)
  (interactive (if current-prefix-arg
		   (list (setq snapshot-name 
			       (expand-file-name 
				(read-file-name "Snapshot to: "))))))
  (if snapshot-name
      (send-to-smalltalk (format "ObjectMemory snapshot: '%s'\n" "Snapshot"))
  (send-to-smalltalk "ObjectMemory snapshot\n" "Snapshot")))

(defun smalltalk-quit ()
  "Terminate the Smalltalk session and associated process.  Emacs remains
running."
  (interactive)
  (send-to-smalltalk "! ! ObjectMemory quit!" "Quitting"))

(defun smalltalk-filein (filename)
  "Do a FileStream>>fileIn: on FILENAME."
  (interactive "fSmalltalk file to load: ")
  (send-to-smalltalk (format "FileStream fileIn: '%s'\n"
			     (expand-file-name filename))
		     "fileIn"))

(defun smalltalk-filein-buffer ()
  (interactive)
  (send-to-smalltalk (buffer-string) "fileIn" (smalltalk-pos 1 1)))

(defun smalltalk-toggle-decl-tracing ()
  (interactive)
  (send-to-smalltalk
   "Smalltalk declarationTrace: Smalltalk declarationTrace not\n"))

(defun smalltalk-toggle-exec-tracing ()
  (interactive)
  (send-to-smalltalk 
   "Smalltalk executionTrace: Smalltalk executionTrace not\n"))


(defun smalltalk-toggle-verbose-exec-tracing ()
  (interactive)
  (send-to-smalltalk
   "Smalltalk verboseTrace: Smalltalk verboseTrace not\n"))

(defun send-to-smalltalk (str &optional mode fileinfo)
    (save-window-excursion
      (gst gst-program-name)
      (save-excursion
	(goto-char (point-max))
	(beginning-of-line)
	(if (looking-at smalltalk-prompt-pattern)
	    (progn (end-of-line)
		   (insert "\n"))))

      (if mode (setq mode-status mode))

      (if fileinfo
	(let (temp-file buf switch-back old-buf)
	  (setq temp-file (concat "/tmp/" (make-temp-name "gst")))
	  (save-excursion
	    (setq buf (get-buffer-create " zap-buffer "))
	    (set-buffer buf)
	    (erase-buffer)
	    (princ str buf)
	    (write-region (point-min) (point-max) temp-file nil 'no-message)
	    )
	  (kill-buffer buf)
	  (process-send-string
	   *smalltalk-process*
	   (format
	    "FileStream fileIn: '%s' line: %d from: '%s' at: %d\n"
	    temp-file (nth 0 fileinfo) (nth 1 fileinfo) (nth 2 fileinfo))))
        (comint-send-string *smalltalk-process* str))
      (switch-to-buffer-other-window (process-buffer *smalltalk-process*))))


(provide 'gst-mode)
