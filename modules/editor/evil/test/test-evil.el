;; -*- no-byte-compile: t; -*-
;;; editor/evil/test/test-evil.el

(describe "editor/evil"
  :var (resv project-root)

  (require! :editor evil)
  (require 'evil)
  (load! "../autoload/evil")

  (before-each
    (fset 'resv #'+evil-resolve-vim-path-a)
    (spy-on 'doom-project-root :and-call-fake (lambda () project-root)))

  ;; `evil-ex-replace-special-filenames' / `+evil-resolve-vim-path-a'
  (describe "file modifiers"
    (it "supports basic vim file modifiers"
      (let ((buffer-file-name  "~/.emacs.d/test/modules/feature/test-evil.el")
            (default-directory "~/.emacs.d/test/modules/")
            (project-root "~/.emacs.d/"))
        (expect (resv "%")   :to-equal "feature/test-evil.el")
        (expect (resv "%:r") :to-equal "feature/test-evil")
        (expect (resv "%:r.elc") :to-equal "feature/test-evil.elc")
        (expect (resv "%:e") :to-equal "el")
        (expect (resv "%:p") :to-equal (expand-file-name buffer-file-name))
        (expect (resv "%:h") :to-equal "feature")
        (expect (resv "%:t") :to-equal "test-evil.el")
        (expect (resv "%:.") :to-equal "feature/test-evil.el")
        (expect (resv "%:~") :to-equal "~/.emacs.d/test/modules/feature/test-evil.el")
        (expect (file-truename (resv "%:p"))
                :to-equal (file-truename buffer-file-name))))

    (it "supports nested vim file modifiers"
      (let ((buffer-file-name  "~/vim/src/version.c")
            (default-directory "~/vim/")
            (project-root "~/vim/"))
        (expect (resv "%:p")     :to-equal (expand-file-name "~/vim/src/version.c"))
        (expect (resv "%:p:.")   :to-equal "src/version.c")
        (expect (resv "%:p:~")   :to-equal "~/vim/src/version.c")
        (expect (resv "%:h")     :to-equal "src")
        (expect (resv "%:p:h")   :to-equal (expand-file-name "~/vim/src"))
        (expect (resv "%:p:h:h") :to-equal (expand-file-name "~/vim"))
        (expect (resv "%:t")     :to-equal "version.c")
        (expect (resv "%:p:t")   :to-equal "version.c")
        (expect (resv "%:r")     :to-equal "src/version")
        (expect (resv "%:p:r")   :to-equal (expand-file-name "~/vim/src/version"))
        (expect (resv "%:t:r")   :to-equal "version")))

    (it "cleans up empty file modifiers"
      (let (buffer-file-name default-directory)
        (expect (resv "%")   :to-equal "")
        (expect (resv "%:r") :to-equal "")
        (expect (resv "%:e") :to-equal "")
        (expect (resv "%:h") :to-equal "")
        (expect (resv "%:t") :to-equal "")
        (expect (resv "%:.") :to-equal "")
        (expect (resv "%:~") :to-equal "")
        (expect (resv "%:P") :to-equal "")))

    (it "supports substitution modifiers"
      (let ((buffer-file-name  "~/.emacs.d/test/modules/feature/test-evil.el")
            (default-directory "~/.emacs.d/test/modules/"))
        (expect (resv "%:s?e?x?")  :to-equal "fxature/test-evil.el")
        (expect (resv "%:gs?e?x?") :to-equal "fxaturx/txst-xvil.xl")))

    (it "cleans up empty substitution modifiers"
      (let (buffer-file-name default-directory)
        (expect (resv "%:s?e?x?")  :to-equal "")
        (expect (resv "%:gs?e?x?") :to-equal "")))))
