;;; feature/jump/config.el

;; "What am I looking at?"
;;
;; This module helps you answer that question. It helps you look up whatever
;; you're looking at, with:
;;
;; 1. A dwim Jump-to-definition functionality that "just works", with the help
;;    of `dumb-jump'.
;; 2. A dwim interface to the new (and experimental) xref API built into Emacs.
;;    Once its API is more stable, backends could be written (or provided by
;;    plugins) to create universal find-references and find-definition
;;    functionality. Warning: xref may change drastically in future updates.
;; 3. Simple ways to look up the symbol at point in external resources, like
;;    stackoverflow, devdocs.io or google.
;; 4. TODO Automatic and transparent integration with cscope databases and ctags
;;    files. Databases are optionally isolated to the Emacs environment.

(defvar +lookup-search-url-alist
  '(("Google"        . "https://google.com/?q=%s")
    ("DuckDuckGo"    . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"    . "http://devdocs.io/#q=%s")
    ("StackOverflow" . "https://stackoverflow.com/search?q=%s"))
  "An alist that maps online resources to their search url.")

(set! :popup "*xref*" :size 10 :noselect t :autokill t :autoclose t)

;; Let me control what backends to fall back on
(setq-default xref-backend-functions '())

(def-setting! :xref-backend (mode function)
  "TODO"
  `(add-hook! ,mode
     (add-hook 'xref-backend-functions #',function nil t)))

;; Recenter after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook)
  'recenter)


;;
;; Packages
;;

(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look dumb-jump-back)
  :config
  (setq dumb-jump-default-project doom-emacs-dir))


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

