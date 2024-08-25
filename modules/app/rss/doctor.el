;;; app/rss/doctor.el -*- lexical-binding: t; -*-

(when (modulep! +youtube)
  (unless (executable-find "mpv")
    (warn! "Couldn't find the mpv executable. Live transcripts with elfeed-tube-mpv will not work."))

  (unless (or (executable-find "youtube-dl")
              (executable-find "yt-dlp"))
    (warn! "Couldn't find the 'youtube-dl' or 'yt-dlp' executables. Live transcripts with elfeed-tube-mpv will not work.")))
