;;; ../test/modules/feature/test-evil.el

(require! :feature evil t)

(def-test-group! feature/evil
  (ert-deftest custom-file-modifiers ()
    (let ((buffer-file-name "~/.emacs.d/test/modules/feature/test-evil.el")
          (default-directory "~/.emacs.d/test/modules"))
      (should (equal (+evil*ex-replace-special-filenames "%") "feature/test-evil.el"))
      (should (equal (+evil*ex-replace-special-filenames "%:r") "feature/test-evil"))
      (should (equal (+evil*ex-replace-special-filenames "%:e") "el"))
      ;; TODO :h
      ;; TODO :t
      ;; TODO :.
      ;; TODO :~
      ;; TODO :s
      ;; TODO :S
      (should (equal (+evil*ex-replace-special-filenames "%:P") (doom-project-root)))
      (should (equal (file-truename (+evil*ex-replace-special-filenames "%:p"))
                     (file-truename buffer-file-name))))))
