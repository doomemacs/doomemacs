
(unless (or (executable-find "aspell")
            (executable-find "hunspell"))
  (warn! "Could not find aspell or hunspell. Flyspell will fall back to ispell, which may not work."))
