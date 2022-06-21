;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/org/doctor.el

(when (featurep! +gnuplot)
  (unless (executable-find "gnuplot")
    (warn! "Couldn't find gnuplot. org-plot/gnuplot will not work")))

(when (featurep! +roam)
  (unless (executable-find "sqlite3")
    (warn! "Couldn't find the sqlite3 executable. org-roam will not work.")))
(when (or (featurep! +roam)
          (featurep! +roam2))
  (unless (executable-find "dot")
    (warn! "Couldn't find the dot executable (from graphviz). org-roam will not be able to generate graph visualizations.")))

(when (featurep! +dragndrop)
  (when IS-MAC
    (unless (executable-find "pngpaste")
      (warn! "Couldn't find the pngpaste executable. org-download-clipboard will not work.")))
  (when IS-LINUX
    (unless (or (executable-find "maim") (executable-find "scrot") (executable-find "gnome-screenshot"))
      (warn! "Couldn't find the maim, scrot or gnome-screenshot executable. org-download-clipboard will not work."))
    (if (string= "wayland" (getenv "XDG_SESSION_TYPE"))
        (unless (executable-find "wl-paste")
          (warn! "Couldn't find the wl-paste executable (from wl-clipboard). org-download-clipboard will not work."))
      (unless (executable-find "xclip")
        (warn! "Couldn't find the xclip executable. org-download-clipboard will not work."))))
  (when IS-WINDOWS
    (unless (executable-find "convert")
      (warn! "Couldn't find the convert program (from ImageMagick). org-download-clipboard will not work."))))

(when (featurep! +media-note)
  (unless (executable-find "mpv")
    (warn! "Couldn't find the mpv executable. org-media-note will not be able to play media files."))
  (unless (executable-find "youtube-dl")
    (warn! "Couldn't find the youtube-dl executable. mpv will not be able to play online media files properly. You can install youtuble-dl using pip or OS package managers like homebrew, apt, dnf(yum), yay and etc.")))
