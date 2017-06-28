;;; lang/org/+publish.el -*- lexical-binding: t; -*-

;; Created by: Matthew Graybosch (public@matthewgraybosch.com)
;; Created on: 26 June 2017
;; Released under the MIT license.

;; The following is a basic org-publish configuration for generating
;; a static HTML5 website and blog with RSS feed from a directory of
;; org-mode files, HTML partials, and static assets.

;; To use this configuration, the user should create a private module
;; under ~/.emacs.d/modules/private/$USERNAME and set the following
;; variables inside an init.el file.

;; (setq user-website "https://www.matthewgraybosch.com")
;; (setq org-publish-source "~/work/website/")
;; (setq org-publish-partials "~/work/website/partials/")
;; (setq org-publish-target "~/public_html/")

;; Taken from Xah Lee's ErgoEmacs: http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; We need this hook to make sure the config function below loads on startup.
(add-hook '+org-init-hook #'+org|init-publish t)

;; Now the fun begins...
(defun +org|init-publish ()
  ;; Set this to true if you want incremental exports. Chances are you do.
  (setq org-publish-use-timestamps-flag nil)

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
           :components ("pages" "posts" "assets" "feed"))
          ("pages"
           :base-directory ,org-publish-source
           :base-extension "org"
           :publishing-directory ,org-publish-target
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :with-toc nil
           :recursive t
           :html-head ,(get-string-from-file (expand-file-name "head.html" org-publish-partials))
           :html-head-extra ,(get-string-from-file (expand-file-name "head-extra.html" org-publish-partials))
           :html-preamble ,(get-string-from-file (expand-file-name "preamble.html" org-publish-partials))
           :html-postamble ,(get-string-from-file (expand-file-name "postamble.html" org-publish-partials))
          ("posts"
           :base-directory ,(concat org-publish-source "posts/")
           :base-extension "org"
           :publishing-directory ,org-publish-target
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :with-toc t
           :recursive t
           :html-head ,(get-string-from-file (expand-file-name "head.html" org-publish-partials))
           :html-head-extra ,(get-string-from-file (expand-file-name "head-extra.html" org-publish-partials))
           :html-preamble ,(get-string-from-file (expand-file-name "preamble.html" org-publish-partials))
           :html-postamble ,(get-string-from-file (expand-file-name "postamble.html" org-publish-partials))
          ("assets"
           :base-directory ,(concat org-publish-source "assets/")
           :base-extension "txt\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
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
           :publishing-function org-rss-publish-to-rss))))))
