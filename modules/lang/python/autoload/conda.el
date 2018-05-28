;;; lang/python/autoload/conda.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/set-conda-home ()
    (interactive)
    (ivy-read "Set conda home:" +python-conda-home
              :history +python/set-conda-home--history
              :action (lambda (cand) (setq conda-anaconda-home cand))))

;;;###autoload
(defun +python|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if conda-env-current-name
              (format "Py:conda:%s" conda-env-current-name)
            "Python")))

;;;###autoload
(defun *anaconda-mode-bootstrap (&optional callback)
  "Run `anaconda-mode' server.
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
