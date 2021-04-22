;;; lang/java/autoload/repl.el -*- lexical-binding: t; -*-

(defvar +java/java-buffer nil)

;;;###autoload
(defun +java/run-java (cmd)
  "Run an inferior Java process.

With argument, allows you to edit the command line (default is
value of `+java/inferior-java-program')."
  (interactive
   (list
    (if current-prefix-arg
        (read-shell-command "Run java: " +java/inferior-java-program)
      +java/inferior-java-program)))
  (let ((buffer
         (if (comint-check-proc +java/java-buffer)
             +java/java-buffer
           (make-comint +java/inferior-java-buffer-name-sans*
                        (or cmd +java/inferior-java-program)))))
    (with-current-buffer buffer
      (+java/inferior-java-mode)
      (setq +java/java-buffer (buffer-name)))
    (pop-to-buffer buffer)
    (get-buffer-process buffer)))

;;;###autoload
(defun +java/open-repl ()
  "Open a Java REPL."
  (interactive)
  (call-interactively #'+java/run-java)
  (get-buffer +java/java-buffer))

(define-derived-mode +java/inferior-java-mode comint-mode "Inferior Java"
  "Major mode for interacting with an inferior Java process.
Runs a Java interpreter as a subprocess of Emacs, with Java I/O
through an Emacs buffer. Variable `+java/inferior-java-program'
controls which Java interpreter is run.

The following commands are available:
\\{+java/inferior-java-mode-map}."
  (compilation-shell-minor-mode 1))
