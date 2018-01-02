;; -*- no-byte-compile: t; -*-
;;; lang/org/test/autoload-org.el

(defmacro should-org-buffer!! (source expected &rest body)
  `(should-buffer!! ,source ,expected
     (org-mode)
     ,@body))

;;
;; `+org/insert-item'
(def-test! insert-item-h1
  "Should append/prepend new first-level headers with an extra newline."
  (should-org-buffer!! ("* {0}Header") ("* Header\n\n* {|}")
    (+org/insert-item 'below))
  (should-org-buffer!! ("* {0}Header") ("* {|}\n\n* Header")
    (+org/insert-item 'above)))

(def-test! insert-item-h2
  "Should append/prepend new second-level (and higher) headers without an extra
newline."
  (should-org-buffer!! ("** {0}Header") ("** Header\n** {|}")
    (+org/insert-item 'below))
  (should-org-buffer!! ("** {0}Header") ("** {|}\n** Header")
    (+org/insert-item 'above)))

(def-test! insert-item-plain-list
  "Should append/prepend new second-level (and higher) headers without an extra
newline."
  (should-org-buffer!! ("+ {0}List item") ("+ List item\n+ {|}")
    (+org/insert-item 'below))
  (should-org-buffer!! ("+ {0}List item"
                       "  + Sub item")
                      ("+ List item"
                       "  + Sub item"
                       "+ {|}")
    (+org/insert-item 'below))
  (should-org-buffer!! ("+ {0}List item"
                       "+ Next item")
                      ("+ List item"
                       "+ {|}"
                       "+ Next item")
    (+org/insert-item 'below)))
