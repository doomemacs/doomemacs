;;; tools/lookup/config.el -*- lexical-binding: t; -*-

;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/file': open the file referenced at point
;;   + `+lookup/online'; look up a symbol on online resources
;;   + `+lookup/in-docsets': look up in Dash docsets
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-provider-url-alist
  '(("Google"            . "https://google.com/search?q=%s")
    ("Google images"     . "https://www.google.com/images?q=%s")
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
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-xref-references-backend
    +lookup-project-search-backend)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions ()
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")


;;
;;; dumb-jump

(def-package! dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector
        (cond ((featurep! :completion ivy)  'ivy)
              ((featurep! :completion helm) 'helm)
              ('popup)))
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))


;;
;;; xref

(after! xref
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defun +lookup*projectile-find-tag (orig-fn)
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall orig-fn)))
  (advice-add #'projectile-find-tag :around #'+lookup*projectile-find-tag)

  ;; The lookup commands are superior, and will consult xref if there are no
  ;; better backends available.
  (global-set-key [remap xref-find-definitions] #'+lookup/definition)
  (global-set-key [remap xref-find-references]  #'+lookup/references)

  ;; Use `better-jumper' instead of xref's marker stack
  (advice-add #'xref-push-marker-stack :around #'doom*set-jump)

  (def-package! ivy-xref
    :when (featurep! :completion ivy)
    :config
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (set-popup-rule! "^\\*xref\\*$" :ignore t))

  (def-package! helm-xref
    :when (featurep! :completion helm)
    :config (setq xref-show-xrefs-function #'helm-xref-show-xrefs)))


;;
;;; Dash docset integration

(def-package! dash-docs
  :when (featurep! +docsets)
  :init
  (add-hook '+lookup-documentation-functions #'+lookup-dash-docsets-backend)
  :config
  (setq dash-docs-enable-debugging doom-debug-mode
        dash-docs-docsets-path (concat doom-etc-dir "docsets/")
        dash-docs-min-length 2
        dash-docs-browser-func #'eww)

  ;; Before `gnutls' is loaded, `gnutls-algorithm-priority' is treated as a
  ;; lexical variable, which breaks `+lookup*fix-gnutls-error'
  (defvar gnutls-algorithm-priority)
  (defun +lookup*fix-gnutls-error (orig-fn url)
    "Fixes integer-or-marker-p errors emitted from Emacs' url library,
particularly, the `url-retrieve-synchronously' call in
`dash-docs-read-json-from-url'. This is part of a systemic issue with Emacs 26's
networking library (fixed in Emacs 27+, apparently).

See https://github.com/magit/ghub/issues/81"
    (let ((gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
      (funcall orig-fn url)))
  (advice-add #'dash-docs-read-json-from-url :around #'+lookup*fix-gnutls-error)

  (def-package! helm-dash
    :when (featurep! :completion helm))

  (def-package! counsel-dash
    :when (featurep! :completion ivy)))
