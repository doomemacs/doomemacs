;;; lib-demo.el

;; This library offers:
;;   + impatient-mode: for broadcasting my emacs session
;;   + TODO integration with reveal.js for presentations
;;   + TODO "big-mode", for making emacs presentable for screencasts/share
;;   + TODO quick note/time keeping for live/youtube recordings

(use-package impatient-mode
  :defer t
  :commands httpd-start)

(provide 'lib-demo)
;;; lib-demo.el ends here
