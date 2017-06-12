;;; test/modules/feature/test-password-store.el

(require! :tools password-store t)

;;
(def-test-group! tools/password-store
  (ert-deftest get-field ()
    (let ((data `((secret . "defuse-account-gad")
                  ("login" . "HL2532-GANDI")
                  ("alt-login" . "hlissner")
                  ("email" . "henrik@lissner.net")
                  ("url" . "https://www.gandi.net/login"))))
      (should (equal (+pass-get-field data "login")
                     "HL2532-GANDI"))
      (should (equal (+pass-get-field data "email")
                     "henrik@lissner.net"))
      (should (equal (+pass-get-field data '("alt-login" "email"))
                     "hlissner"))
      (should (equal (+pass-get-field data '("username" "email"))
                     "henrik@lissner.net"))
      (should-not (+pass-get-field data '("x" "y" "z"))))

    (should-error (+pass-get-field nil nil))))
