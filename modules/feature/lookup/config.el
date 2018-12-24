;;; feature/lookup/config.el -*- lexical-binding: t; -*-

;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/online'; look up a symbol on online resources
;;   + `+lookup/in-docsets': look up in Dash docsets
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-provider-url-alist
  '(("Google"            . "https://google.com/search?q=%s")
    ("Google images"     . "https://google.com/images?q=%s")
    ("Google maps"       . "https://maps.google.com/maps?q=%s")
    ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("DuckDuckGo"        . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"        . "https://devdocs.io/#q=%s")
    ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
    ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
    ("Youtube"           . "https://youtube.com/results?aq=f&oq=&search_query=%s")
    ("Wolfram alpha"     . "https://wolframalpha.com/input/?i=%s")
    ("Wikipedia"         . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"))
  "An alist that maps online resources to their search url or a function that
produces an url. Used by `+lookup/online'.")

(defvar +lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar +lookup-definition-functions
  '(+lookup-xref-definitions-backend
    +lookup-dumb-jump-backend
    +lookup-project-search-backend
    +lookup-evil-goto-definition-backend)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

(defvar +lookup-references-functions
  '(+lookup-xref-references-backend
    +lookup-project-search-backend)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

(defvar +lookup-documentation-functions
  '(+lookup-dash-docsets-backend
    +lookup-online-backend)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

(defvar +lookup-file-functions ()
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point.")

;; Recenter buffer after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook
    counsel-grep-post-action-hook dumb-jump-after-jump-hook)
  #'recenter)


;;
;; dumb-jump

(def-package! dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector
        (cond ((featurep! :completion ivy)  'ivy)
              ((featurep! :completion helm) 'helm)
              ('popup))))


;;
;; xref

;; By default, `etags--xref-backend' is the default xref backend. No need. We'll
;; set these up ourselves in other modules.
(setq-default xref-backend-functions '(t))

;; ...however, it breaks `projectile-find-tag', unless we put it back.
(defun +lookup*projectile-find-tag (orig-fn)
  (let ((xref-backend-functions '(etags--xref-backend t)))
    (funcall orig-fn)))
(advice-add #'projectile-find-tag :around #'+lookup*projectile-find-tag)


(def-package! ivy-xref
  :when (featurep! :completion ivy)
  :after xref
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


(def-package! helm-xref
  :when (featurep! :completion helm)
  :after xref
  :config (setq xref-show-xrefs-function #'helm-xref-show-xrefs))


;;
;; Dash docset integration

;; Both packages depend on helm-dash, for now
(def-package! helm-dash
  :when (featurep! +docsets)
  :defer t
  :init
  (setq helm-dash-enable-debugging doom-debug-mode
        helm-dash-browser-func #'eww)
  :config
  (unless (file-directory-p helm-dash-docsets-path)
    (setq helm-dash-docsets-path (concat doom-etc-dir "docsets/")))
  (unless (file-directory-p helm-dash-docsets-path)
    (make-directory helm-dash-docsets-path t)))

(def-package! counsel-dash
  :when (and (featurep! +docsets)
             (featurep! :completion ivy))
  :commands counsel-dash-install-docset
  :config (setq counsel-dash-min-length 2))
