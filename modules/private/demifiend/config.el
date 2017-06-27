;;; lang/org/+publish.el -*- lexical-binding: t; -*-

;; Default config for org-publish.
;; Contributed by Matthew Graybosch (public@matthewgraybosch.com)
(setq user-login-name "demifiend")
(setq user-full-name "Matthew Graybosch")
(setq user-email-address "public@matthewgraybosch.com")

(defvar user-website "https://www.matthewgraybosch.com")
(defvar org-publish-source "~/work/website/")
(defvar org-publish-partials "~/work/website/partials/")
(defvar org-publish-target "~/public_html/")

;; Set default settings outside of publish to reduce redundancy.
;; Users can override these on a per-file basis.
(setq org-export-with-inlinetasks nil
      org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-statistics-cookies nil
      org-export-with-tasks nil)

;; HTML settings
(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-container-element "section"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      org-html-htmlize-output-type 'css
      org-html-head-include-default-style t
      org-html-head-include-scripts t
      org-html-doctype "html5"
      org-html-home/up-format "%s\n%s\n")

;; Project Definition
(setq org-publish-project-alist
      `(("website"
         :components ("pages" "posts" "assets"))
        ("pages"
         :base-directory ,org-publish-source
         :base-extension "org"
         :publishing-directory ,org-publish-target
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :recursive t
         :html-head ,(concat org-publish-partials "head.html")
         :html-head-extra ,(concat org-publish-partials "head-extra.html")
         :html-preamble ,(concat org-publish-partials "preamble.html")
         :html-postamble, (concat org-publish-partials "postamble.html"))
        ("posts"
         :base-directory ,(concat org-publish-source "posts/")
         :base-extension "org"
         :publishing-directory ,org-publish-target
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t
         :recursive t
         :html-head ,(concat org-publish-partials "head.html")
         :html-head-extra ,(concat org-publish-partials "head-extra.html")
         :html-preamble ,(concat org-publish-partials "preamble.html")
         :html-postamble, (concat org-publish-partials "postamble.html"))
        ("assets"
         :base-directory ,(concat org-publish-source "assets/")
         :base-extension "txt\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,org-publish-target
         :publishing-function org-publish-attachment
         :recursive t)
        ("feed"
         :base-directory ,(concat org-publish-source "posts/")
         :base-extension "org"
         :exclude ".*"
         :exclude-tags ("noexport" "norss")
         :include ("index.org")
         :html-link-home ,user-website
         :html-link-use-abs-url t
         :publishing-directory ,(concat org-publish-target "/feed/")
         :publishing-function org-rss-publish-to-rss)))
