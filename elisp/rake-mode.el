;; -*-Emacs-Lisp-*-
;;
;; rake-mode.el --- in-file Rake task running
;;
;; Copyright (C) 2008 Joseph Abrahamson <abrahamson dot j at gmail dot com>.
;; Released under The Perl Artistic License 2.0
;;

;; Code:

(require 'ido)
(provide 'rake-mode)

(defvar rake-minor-mode-map (make-sparse-keymap) "Keymap for Rake minor mode")

(define-minor-mode rake-mode
  "Toggle Rake minor mode

With no argument, this command toggles Rake mode. Non-null prefix
arguments turn the mode on and null deactive it.

Rake mode enables Rake task calling with autocompletion in the
specified directory."
  :init-value nil
  :lighter " Rake"
  :keymap rake-minor-mode-map
  nil)


(defvar rake-mode/rakefile nil
  "* Path to Rakefile. Rake will be executed here.")

(defun rake-mode/task (task &optional prefix)
  "Run a Rake task from the tasks described by Rakefile.

If no Rakefile is described use rake-mode/visit-rakefile.

If prefix is given, the output from Rake is sent to the
minibuffer temporarily instead of being given its own temp buffer"
  (interactive
   (list (ido-completing-read (concat "Tasks at (" rake-mode/rakefile "): ") (rake-mode/get-tasks))
	 current-prefix-arg))
  (when task
    (save-current-buffer
      (let ((str (concat "*** RAKE TASK: "
			 task
			 " ***\n"
			 (ansi-color-apply
			  (in-rakepath
			   (shell-command-to-string
			    (concat "rake "
				    task)))))))
	(if prefix
	    (message str)
	  (with-output-to-temp-buffer "*Rake Output*"
	    (princ str)))))))

(defun rake-mode/visit-rakefile (file &optional local)
  (interactive (list (ido-read-file-name "Visit Rakefile (default Rakefile): "
				     default-directory
				     (expand-file-name "Rakefile"
						       default-directory)
				     t)
		     current-prefix-arg))
  (or (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
  (let ((rakefile-name file))
    (save-excursion
      (or (find-file-noselect file)
	  (signal 'file-error (list "Visiting Rakefile"
				    "file does not exist"
				    file)))))
  (if local
      (set (make-local-variable 'rake-mode/rakefile) file)
    (setq-default rake-mode/rakefile file)))

(defmacro in-rakepath (&rest body)
  `(let ((default-directory (rake-mode/get-rakefile-path))) ,@body))

(defun rake-mode/get-rakefile-path ()
  (if rake-mode/rakefile
      (replace-regexp-in-string "\/\[^/\]\+$" "/" rake-mode/rakefile)
    (error "Rakefile not yet visited.")))

(defun rake-mode/get-tasks ()
  (in-rakepath
   (let ((lines (split-string (shell-command-to-string "rake -P") "\n")))
     (if (string= (car lines) "rake aborted!")
         (error "Rake failed. Check Rakefile"))
     (loop for str in lines
           for task = (save-match-data
                        (when (and (not (string= "" str))
                                   (not (string-match "^ +$" str)))
                          (nth 1 (s-match "^rake \\([^ ]*\\)" str))
                          ))
           when task collect task))))
