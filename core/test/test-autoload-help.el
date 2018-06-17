;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-help.el

(load! "autoload/help" doom-core-dir)

;;
(describe "core/autoload/help"
  :var (a)
  (before-each (setq a (switch-to-buffer (get-buffer-create "a"))))
  (after-each  (kill-buffer a))

  (describe "what-face"
    (before-each
      (insert (propertize "Hello " 'face 'font-lock-keyword-face))
      (insert "world"))
    (it "returns list of faces at point"
      (expect (doom/what-face nil (point-min)) :to-equal '((font-lock-keyword-face) ())))
    (it "returns nil if no faces at point"
      (expect (doom/what-face nil (point-max)) :to-be nil)))

  (describe "what-face overlays"
    (before-each
      (insert "Hello world")
      (let ((ov (make-overlay 1 6)))
        (overlay-put ov 'face 'font-lock-keyword-face)))

    (it "returns list of overlays at point"
      (expect (doom/what-face nil (point-min)) :to-equal '(() (font-lock-keyword-face))))
    (it "returns nil if no overlays at point"
      (expect (doom/what-face nil (point-max)) :to-be nil))))

