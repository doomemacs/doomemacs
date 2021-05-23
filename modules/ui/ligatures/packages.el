(when (and (or (featurep 'ns)
               (string-match-p "HARFBUZZ" system-configuration-features))
           (featurep 'composite))
  (package! ligature
    :pin "0e5d0a8554622bcb0ec634e364795650ff4f2457"))
