;;; module-demo.el --- for collaboration and demonstrations

;; This library offers:
;;   + impatient-mode: for broadcasting my emacs session
;;   + TODO integration with reveal.js for presentations
;;   + TODO "big-mode", for making emacs presentable for screencasts/share
;;   + TODO quick note/time keeping for live/youtube recordings

(use-package impatient-mode
  :defer t
  :commands httpd-start)

(use-package puml-mode
  :mode "\\.p\\(lant\\)?uml$"
  :init
  (setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8029/plantuml.8029.jar"))

;;;

(defvar big-mode-font narf-default-font)

(define-minor-mode big-mode
  :init-value nil
  :lighter " BIG"
  :global t
  (narf/load-font (if big-mode big-mode-font narf-default-font)))

(provide 'module-demo)
;;; module-demo.el ends here
