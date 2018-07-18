;;; lang/python/+conda.el -*- lexical-binding: t; -*-
;;;###if (featurep! +conda)

;; Adds conda support to Doom Emacs. `conda-anaconda-home' should be the path to
;; your anaconda installation, and will be guessed from the following:
;;
;; + ~/.anaconda3
;; + ~/.anaconda
;; + ~/usr/bin/anaconda3
;;
;; If none of these work, you'll need to set `conda-anaconda-home' yourself.
;;
;; Once set, run M-x `conda-env-activate' to switch between environments OR turn
;; on `conda-env-autoactivate-mode' if you want it done automatically.

(def-package! conda
  :when (featurep! +conda)
  :after python
  :config
  (unless (cl-loop for dir in (list conda-anaconda-home "/usr/bin/anaconda3" "~/.anaconda")
                   if (file-directory-p dir)
                   return (setq conda-anaconda-home dir
                                conda-env-home-directory dir))
    (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (after! eshell (conda-env-initialize-eshell))

  (add-hook! '(conda-postactivate-hook conda-postdeactivate-hook)
      #'+python|add-conda-env-to-modeline)

  (advice-add 'anaconda-mode-bootstrap :override #'+python*anaconda-mode-bootstrap-in-remote-environments))
