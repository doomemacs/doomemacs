(use-package erlang-start
  :mode (("\\.erl?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode))
  :config
  (progn
    (setq erlang-root-dir "/opt/local/lib/erlang")
    (setq erlang-man-root-dir "/opt/local/lib/erlang/man")
    (add-to-list 'exec-path "/opt/local/lib/erlang/bin")))

;; TODO Install distel
;; TODO Esense?

(provide 'init-erlang)
;;; init-erlang.el ends here
