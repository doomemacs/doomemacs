;;; lang/java/config.el -*- lexical-binding: t; -*-

(add-hook 'java-mode-hook #'rainbow-delimiters-mode)

(cond ((featurep! +meghanada) (load! +meghanada))
      ((featurep! +eclim) ; FIXME lang/java +eclim
       ;;(load! +eclim)
       (warn "java-mode: eclim support isn't implemented yet")))


;;
;; Common plugins
;;

(def-package! android-mode
  :commands android-mode
  :init
  (add-hook! (java-mode groovy-mode nxml-mode) #'+java|android-mode-maybe)
  :config
  (set! :yas-minor-mode 'android-mode)
  (set! :company-dict-minor-mode 'android-mode))


(def-package! groovy-mode
  :mode "\\.g\\(radle\\|roovy\\)$"
  :config
  (set! :eval 'groovy-mode "groovy"))

