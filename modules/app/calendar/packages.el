;; -*- no-byte-compile: t; -*-
;;; app/calendar/packages.el

(when (package! calfw :pin "36846cdca91794cf38fa171d5a3ac291d3ebc060")
  (package! calfw-org)   ; part of calfw
  (package! calfw-cal)   ; part of calfw
  (package! calfw-ical)) ; part of calfw
(package! org-gcal :pin "0f46c08f60355729526e970e370defe624e84956")
