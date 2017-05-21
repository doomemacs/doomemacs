;;; feature/jump/config.el

;; "What am I looking at?"
;;
;; This module helps you answer that question. It helps you look up whatever
;; you're looking at.
;;
;;   + `+jump/definition': a jump-to-definition that should 'just work'
;;   + `+jump/references': find a symbol's references in the current project
;;   + `+jump/online'; look up a symbol on online resources, like stackoverflow,
;;     devdocs.io or google.
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +jump-search-url-alist
  '(("Google"        . "https://google.com/search?q=%s")
    ("DuckDuckGo"    . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"    . "http://devdocs.io/#q=%s")
    ("StackOverflow" . "https://stackoverflow.com/search?q=%s"))
  "An alist that maps online resources to their search url or a function that
produces an url. Used by `+jump/online'.")

(def-setting! :xref-backend (mode function)
  "TODO"
  `(add-hook! ,mode
     (add-hook 'xref-backend-functions #',function nil t)))

(set! :popup "*xref*" :noselect t :autokill t :autoclose t)

;; Let me control what backends to fall back on
(setq-default xref-backend-functions '(t))

;; Recenter after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook counsel-grep-post-action-hook)
  'recenter)


;;
;; Packages
;;

(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look dumb-jump-back)
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-selector (cond ((featurep! :completion ivy) 'ivy)
                                 ((featurep! :completion helm) 'helm)
                                 (t 'popup))))


(def-package! gxref
  :commands (gxref-xref-backend
             gxref-create-db
             gxref-update-db
             gxref-single-update-db
             gxref-set-project-dir)
  :init
  (setq-default xref-backend-functions '(gxref-xref-backend t)))


;; (def-package! ggtags
;;   :commands (ggtags-find-tag-dwim
;;              ggtags-find-tag-mouse
;;              ggtags-find-definition
;;              ggtags-find-reference
;;              ggtags-find-other-symbol
;;              ggtags-find-tag-regexp
;;              ggtags-idutils-query
;;              ggtags-grep
;;              ggtags-find-file
;;              ggtags-query-replace
;;              ggtags-delete-tags
;;              ggtags-explain-tags))

