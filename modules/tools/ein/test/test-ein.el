;; -*- no-byte-compile: t; -*-
;;; tools/ein/test/test-ein.el

(describe "tools/ein"
  :var (project-root)

  (require! :tools ein)
  (require 'ein-notebook)

  (before-each
    (spy-on 'doom-project-root :and-call-fake (lambda () project-root)))

  (it "keymap defs still valid"
      (dolist (km (list ein:notebook-mode-map ein:notebooklist-mode-map))
        (cl-labels ((validate-km
                     (km)
                     (map-keymap (lambda (type def)
                                   (cond ((keymapp def)
                                          (validate-km def))
                                         (t (should-not
                                             (and (symbolp def) (not (commandp def)))))))
                                 km)))
          (validate-km km)))))
