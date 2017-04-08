;;; app/present/config.el

;; Sometimes you just get that powerful urge to show off. I don't have a fancy
;; car, so Emacs will have to do.
;;
;;  + `impatient-mode' lets me show off code, as I write it, in real-time over
;;    HTTP (see `+present/buffer')
;;  + `ox-reveal' adds a reveal.js exporter to org-mode
;;  + and `+present/big-mode' lets me toggle big fonts for streams or
;;    screen-sharing

;; Fonts for `+present/big-mode'
(defvar +present-original-font +doom-font)
(defvar +present-big-font (font-spec :family "Fira Mono" :size 16))


;;
;; Plugins
;;

(def-package! impatient-mode
  :commands impatient-mode)

;; reveal.js
(when (featurep! :lang org)
  (if (bound-and-true-p org-modules-loaded)
      (require 'ox-reveal)
    (add-hook! 'org-load (require 'ox-reveal)))

  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
        org-reveal-mathjax t))

