;;; app/spotify/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(package! helm-spotify-plus :pin "c3922ec368"
  :when (featurep! helm)
  :config
  (map! :localleader
        :map global-map
        :prefix "5"
        "p" #'helm-spotify-plus-previous
        "n" #'helm-spotify-plus-next
        "x" #'helm-spotify-plus-pause
        "q" #'helm-spotify-plus-queue-stop
        "s" #'helm-spotify-plus
        ) :pin "c3922ec368")
