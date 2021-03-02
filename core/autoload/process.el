;;; core/autoload/process.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

;;;###autoload
(defun doom-exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Unlike `doom-call-process', this pipes output to `standard-output' on the fly to
simulate 'exec' in the shell, so batch scripts could run external programs
synchronously without sacrificing their output.

Warning: freezes indefinitely on any stdin prompt."
  ;; FIXME Is there any way to handle prompts?
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "doom-sh"
                               :buffer (current-buffer)
                               :command (cons command (remq nil args))
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (_process output)
                       (princ output (current-buffer))
                       (princ output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))

(defvar doom--num-cpus nil)
;;;###autoload
(defun doom-num-cpus ()
  "Return the max number of processing units on this system.
Tries to be portable. Returns 1 if cannot be determined."
  (or doom--num-cpus
      (setq doom--num-cpus
            (let ((cpus
                   (cond ((getenv "NUMBER_OF_PROCESSORS"))
                         ((executable-find "nproc")
                          (doom-call-process "nproc"))
                         ((executable-find "sysctl")
                          (doom-call-process "sysctl" "-n" "hw.ncpu")))))
              (max
               1 (or (cl-typecase cpus
                       (string
                        (condition-case _
                            (string-to-number cpus)
                          (wrong-type-argument
                           (user-error "NUMBER_OF_PROCESSORS contains an invalid value: %S"
                                       cpus))))
                       (consp
                        (if (zerop (car cpus))
                            (string-to-number (cdr cpus))
                          (user-error "Failed to look up number of processors, because:\n\n%s"
                                      (cdr cpus)))))
                     1))))))
