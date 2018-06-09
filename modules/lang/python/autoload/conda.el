;;; lang/python/autoload/conda.el -*- lexical-binding: t; -*-
;;;###if (featurep! +conda)

;;;###autoload
(defun +python/set-conda-home ()
  "Set the CONDA HOME.
Usually it's `~/.anaconda3' on local machine, but you can also set it to a
remote directory using TRAMP syntax such as `/ssh:host:/usr/bin/anaconda3'. In
that way you can use the remote conda environment as well as the corresponding
remote python executable and packages."
  (interactive)
  (ivy-read "Set conda home:" +python-conda-home
            :history +python/set-conda-home--history
            :action (lambda (cand) (setq conda-anaconda-home cand))))

;;;###autoload
(defun +python|add-conda-env-to-modeline ()
    "Add conda environment string to the major mode in the modeline."
    (setq mode-name
          (if conda-env-current-name
              (format "Py:conda:%s" conda-env-current-name)
            "Python")))
;;;###autoload
(defun +python*anaconda-mode-bootstrap (&optional callback)
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

