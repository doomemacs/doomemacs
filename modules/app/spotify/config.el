;;; app/spotify/config.el -*- lexical-binding: t; -*-

(use-package! helm-spotify-plus
  :when (featurep! helm)
  :config
  (map! :leader
        :map global-map
        :prefix "5"
        "p" #'helm-spotify-plus-previous
        "n" #'helm-spotify-plus-next
        "x" #'helm-spotify-plus-pause
        "q" #'helm-spotify-plus-queue-stop
        "s" #'helm-spotify-plus
        ))
