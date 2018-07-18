;;; lang/python/autoload/conda.el -*- lexical-binding: t; -*-
;;;###if (featurep! +conda)

;;;###autoload
(defun +python/set-conda-home ()
  "Set `conda-anaconda-home' (ANACONDA_HOME).

Usually it's `~/.anaconda3' on local machine, but it can be set to a remote
directory using TRAMP syntax, e.g. `/ssh:host:/usr/bin/anaconda3'. This way, you
can use a remote conda environment, including the corresponding remote python
executable and packages."
  (interactive)
  (require 'conda)
  (when-let* ((home (read-directory-name "Set conda home: " "~" nil nil conda-anaconda-home)))
    (setq conda-anaconda-home home)
    (message "Successfully changed conda home to: %s" (abbreviate-file-name home))))

;;;###autoload
(defun +python|add-conda-env-to-modeline ()
  "Add conda environment string to the major mode modeline segment."
  (setq mode-name
        (if conda-env-current-name
            (format "Py:conda:%s" conda-env-current-name)
          "Python")))

;;;###autoload
(defun +python*anaconda-mode-bootstrap-in-remote-environments (&optional callback)
  "Advice to set up the anaconda-mode even in remote environment.
Original doc:
Run `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
  (setq anaconda-mode-process
        (start-pythonic :process anaconda-mode-process-name
                        :buffer anaconda-mode-process-buffer
                        :cwd (anaconda-mode-server-directory)
                        :filter (lambda (process output) (anaconda-mode-bootstrap-filter process output))
                        :sentinel 'anaconda-mode-bootstrap-sentinel
                        :query-on-exit nil
                        :args (list "-c"
                                    anaconda-mode-server-command
                                    (if (pythonic-remote-p)
                                        "0.0.0.0" "127.0.0.1")
                                    (or (pythonic-file-name pythonic-environment) ""))))
  (process-put anaconda-mode-process 'server-directory (anaconda-mode-server-directory)))

