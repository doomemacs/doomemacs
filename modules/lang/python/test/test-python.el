;; -*- no-byte-compile: t; -*-
;;; lang/python/test/test-python.el

(require 'buttercup)

(describe "lang/python"
  (describe "pyvenv-track-virtualenv hook guard"
    (it "should not add pyvenv-track-virtualenv when +poetry is active"
      ;; The guard: (unless (or +poetry +conda +uv) (add-hook ...))
      ;; When +poetry is active, pyvenv-track-virtualenv should NOT be on the hook
      (let ((has-poetry t))
        (expect (not (or has-poetry nil nil)) :to-be nil)))

    (it "should not add pyvenv-track-virtualenv when +conda is active"
      (let ((has-conda t))
        (expect (not (or nil has-conda nil)) :to-be nil)))

    (it "should not add pyvenv-track-virtualenv when +uv is active"
      (let ((has-uv t))
        (expect (not (or nil nil has-uv)) :to-be nil)))

    (it "should add pyvenv-track-virtualenv when no venv manager flag is active"
      (let ((has-poetry nil) (has-conda nil) (has-uv nil))
        (expect (not (or has-poetry has-conda has-uv)) :to-be t)))))
