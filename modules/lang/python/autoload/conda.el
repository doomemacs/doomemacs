;;; lang/python/autoload/conda.el -*- lexical-binding: t; -*-
;;;###if (modulep! +conda)

;;;###autoload
(defun +python/set-conda-home ()
  "Set `conda-anaconda-home' (ANACONDA_HOME).

Usually it's `~/.anaconda3' on local machine, but it can be set to a remote
directory using TRAMP syntax, e.g. `/ssh:host:/usr/bin/anaconda3'. This way, you
can use a remote conda environment, including the corresponding remote python
executable and packages."
  (interactive)
  (require 'conda)
  (when-let (home (read-directory-name "Set conda home: " "~" nil nil conda-anaconda-home))
    (setq conda-anaconda-home home)
    (message "Successfully changed conda home to: %s" (abbreviate-file-name home))))

