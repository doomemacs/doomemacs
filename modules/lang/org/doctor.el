;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/org/doctor.el

(when (featurep! +gnuplot)
  (unless (executable-find "gnuplot")
    (warn! "Couldn't find gnuplot. org-plot/gnuplot will not work")))

(when (featurep! +roam)
  (unless (executable-find "sqlite3")
    (warn! "Couldn't find the sqlite3 executable. org-roam will not work."))
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
