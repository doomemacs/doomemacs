;; -*- no-byte-compile: t; -*-
;;; app/rss/packages.el

(package! elfeed :pin "a39fb78e34ee25dc8baea83376f929d7c128344f")
(package! elfeed-goodies :pin "544ef42ead011d960a0ad1c1d34df5d222461a6b")
(when (modulep! +org)
  (package! elfeed-org :pin "1197cf29f6604e572ec604874a8f50b58081176a"))
(when (modulep! +youtube)
  (package! elfeed-tube :pin "ce2b5071d153fdda15ad6166a33f1846084d7504"))
