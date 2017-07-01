;;; private/demifiend/init.el -*- lexical-binding: t; -*-

(setq debug-on-error nil)
(setq user-login-name "demifiend")
(setq user-full-name "Matthew Graybosch")
(setq user-email-address "public@matthewgraybosch.com")

(defvar user-website "https://www.matthewgraybosch.com")
(defvar org-publish-source "~/work/website/")
(defvar org-publish-partials "~/work/website/partials/")
(defvar org-publish-target "~/public_html/")

(require 'company)
(setq company-idle-delay 0.1
      company-minimum-prefix-length 3
      company-backends '(company-abbrev company-dabbrev company-capf company-ispell company-yasnippet company-etags company-elisp company-files company-gtags))
