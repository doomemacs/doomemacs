;;; tools/pdf/+modeline.el -*- lexical-binding: t; -*-

(def-modeline-segment! +pdf-pages
  "Current and total page indicator for PDF documents."
  (format " P %d/%d" (pdf-view-current-page) (pdf-cache-number-of-pages)))

(def-modeline! pdf-tools-modeline
  (bar matches " " buffer-info +pdf-pages)
  (major-mode vcs))
