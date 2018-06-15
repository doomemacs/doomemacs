;;; core/autoload/os.el -*- lexical-binding: t; no-byte-compile: t -*-

;; FIXME obsolete :env
;;;###autoload
(def-setting! :env (&rest vars)
  "TODO"
  :obsolete set-env!
  (when (featurep 'exec-path-from-shell)
    `(exec-path-from-shell-copy-envs ,@vars)))
