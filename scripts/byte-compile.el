#!emacs --script

(load (concat user-emacs-directory "init.el"))

(require 'bytecomp)
(byte-recompile-file (expand-file-name "init-load-path.el" narf-emacs-dir) nil 0)
(byte-recompile-file (expand-file-name "init.el" narf-emacs-dir) nil 0)
(byte-recompile-file (expand-file-name "core.el" narf-core-dir) t 0)
(byte-recompile-file (expand-file-name "core-vars.el" narf-core-dir) t 0)
(byte-recompile-file (expand-file-name "core-defuns.el" narf-core-dir) t 0)
(dolist (dir (append (list narf-core-dir narf-contrib-dir)
                     (list (concat narf-modules-dir "lib/")
                           (concat narf-core-dir "lib/")
                           narf-modules-dir)))
  (byte-recompile-directory dir 0 nil))

(byte-recompile-file (expand-file-name "my-bindings.el" narf-core-dir) t 0)
(byte-recompile-file (expand-file-name "my-commands.el" narf-core-dir) t 0)
