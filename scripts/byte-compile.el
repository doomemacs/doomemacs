#!emacs --script

(load (concat user-emacs-directory "bootstrap.el"))

(require 'bytecomp)
(byte-recompile-file (expand-file-name "bootstrap.el" narf-emacs-dir) nil 0)
(byte-recompile-file (expand-file-name "init.el" narf-emacs-dir) nil 0)
(byte-recompile-file (expand-file-name "core.el" narf-core-dir) t 0)
(byte-recompile-file (expand-file-name "core-vars.el" narf-core-dir) t 0)
(byte-recompile-file (expand-file-name "core-defuns.el" narf-core-dir) t 0)
(dolist (dir (list narf-core-dir narf-modules-dir narf-core-dir))
  (byte-recompile-directory dir 0 nil))

(byte-recompile-file (expand-file-name "my-bindings.el" narf-core-dir) t 0)
(byte-recompile-file (expand-file-name "my-commands.el" narf-core-dir) t 0)
