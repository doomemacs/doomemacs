;;; lang/python/autoload/lpy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun *lispy-set-python-process-action (x)
  "ivy action to set python process name of current buffer"
  ;; variable that specify the lispy python process
  (setq-local
   python-shell-buffer-name
   (string-trim
    (buffer-name
     (process-buffer
      (cond ((consp x) (cdr x))
            (t
             (lispy--python-proc
              (concat
               "Python:lispy-python-"
               x)))))) "*" "*"))
  (setq-local
   lispy-python-proc
   (cond ((consp x) (cdr x))
         (t
          (lispy--python-proc
           (concat
            "Python:lispy-python-"
            x))))))

;;;###autoload
(defun *lispy-short-process-name (x)
  "clean up lispy process name"
  (process-name x))

;;;###autoload
(defun *lispy--python-proc (&optional name)
  (let* ((proc-name (or name
                        lispy-python-proc
                        "Python:lispy-python-default"))
         (process (get-process proc-name)))
    (if (process-live-p process)
        process
      (let* ((python-shell-font-lock-enable nil)
             (inferior-python-mode-hook nil)
             (python-shell-interpreter (cond ((save-excursion
                                                (goto-char (point-min))
                                                (looking-at
                                                 "#!\\(?:/usr/bin/env \\)\\(.*\\)$"))
                                              (match-string-no-properties 1))
                                             ((file-exists-p
                                               python-shell-interpreter)
                                              (expand-file-name
                                               python-shell-interpreter))
                                             (t python-shell-interpreter)))
             (python-binary-name (python-shell-calculate-command)))
        (setq process
              (get-buffer-process
               (python-shell-make-comint
                python-binary-name
                proc-name
                nil
                nil))))
      (setq lispy--python-middleware-loaded-p
            nil)
      (lispy--python-middleware-load)
      process)))

