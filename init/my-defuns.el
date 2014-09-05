(provide 'my-defuns)

(add-to-list 'load-path (expand-file-name "defuns" *init-dir))

(shut-up
  (load "defuns/config")
  (load "defuns/commands")
  (load "defuns/text")
  (load "defuns/hooks")
  (load "defuns/utility"))
