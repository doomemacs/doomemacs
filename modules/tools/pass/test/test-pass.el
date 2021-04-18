;; -*- no-byte-compile: t; -*-
;;; tools/pass/test/test-pass.el

(describe "tools/pass"
  (before-all
    (load! "../autoload"))

  (before-each
    (spy-on 'auth-source-pass-parse-entry :and-call-fake
            (lambda (entry)
              (when (equal entry "fake/source")
                '((secret . "defuse-account-gad")
                  ("login" . "HL2532")
                  ("alt-login" . "hlissner")
                  ("email" . "henrik@lissner.net")
                  ("url" . "https://some-place.net/login"))))))

  (describe "get field"
    (it "returns specific fields"
      (expect (+pass-get-field "fake/source" "email")
              :to-equal "henrik@lissner.net"))
    (it "returns first existing of a list of fields"
      (expect (+pass-get-field "fake/source" '("alt-login" "email"))
              :to-equal "hlissner")
      (expect (+pass-get-field "fake/source" '("username" "email"))
              :to-equal "henrik@lissner.net"))
    (it "returns nil for missing fields"
      (expect (+pass-get-field "fake/source" '("x" "y" "z"))
              :to-be nil))
    (it "throws error on missing entries"
      (expect (+pass-get-field "nonexistent/source" "login")
              :to-throw)))

  (describe "get user/secret"
    (it "returns the user"
      (let ((+pass-user-fields '("login" "user" "username" "email")))
        (expect (+pass-get-user "fake/source")
                :to-equal "HL2532")))
    (it "returns the secret"
      (expect (+pass-get-secret "fake/source")
              :to-equal "defuse-account-gad"))))
