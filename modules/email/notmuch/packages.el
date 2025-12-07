;; -*- no-byte-compile: t; -*-
;;; email/notmuch/packages.el

(package! notmuch
  :recipe (:pre-build
           (with-temp-file "emacs/notmuch-version.el"
             (insert-file-contents "emacs/notmuch-version.el.tmpl")
             (re-search-forward "%VERSION%")
             (replace-match
              (format "\"%s+%s~%.7s\""
                      (with-temp-buffer (insert-file-contents "version.txt")
                                        (string-trim (buffer-string)))
                      (save-match-data
                        (let ((desc (doom-call-process "git" "describe" "--abbrev=7" "--match" "[0-9.]*")))
                          (if (zerop (car desc))
                              (car (last (split-string (cdr desc) "-") 2))
                            "??")))
                      (cdr (doom-call-process "git" "rev-parse" "HEAD")))
              t t)))
  :pin "5c921b6c0b2df460c7d50f6563edf700d0420732")

(when (modulep! +org)
  (package! org-mime :pin "ffaad784a8597ee52842a578c01bd347d3e0281d"))
(when (modulep! :lang org)
  (package! ol-notmuch :pin "51deac09857cb6b329cf2c3b899332deba28bad2"))
(when (modulep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935e4180c42524c51609d1283e9be0688"))
(when (modulep! :completion helm)
  (package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288"))
(when (modulep! :completion vertico)
  (package! consult-notmuch :pin "abc0318c9971b4288cc96f6f934ad6d36e63d9f9"))
