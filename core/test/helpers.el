;; -*- no-byte-compile: t; -*-
;;; core/test/helpers.el

(defmacro insert!! (&rest text)
  "Insert TEXT in buffer, then move cursor to last {0} marker."
  `(progn
     (insert ,@text)
     (when (search-backward "{0}" nil t)
       (replace-match "" t t))))
