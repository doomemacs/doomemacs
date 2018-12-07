;;; feature/evil/+everywhere.el -*- lexical-binding: t; -*-

;; We load evil-collection ourselves for these reasons:
;;
;; 1. To truly lazy load it. Some of its modules, like the elisp-mode and
;;    buff-menu ones are loaded immediately, because Emacs loads them
;;    immediately, pulling in all of evil-collection and sometimes other
;;    packages.
;; 2. This ensures a predictable load order, versus lazy loading using :defer or
;;    :after-call. This means users can use (after! org ...) and be sure that
;;    their changes will override evil-collection's.
;; 3. Eventually, I'd like to remove evil-collection. It changes too often,
;;    introduces breaking bugs too frequently, and I don't always agree with
;;    their design choices. Regardless, there are useful tidbits I'd like to
;;    keep. This will be a slow transition, but this file is where most of it
;;    will happen.
;; 4. Adds `+evil-collection-disabled-list', to make it easier for users to
;;    disable modules, and to reduce the effort required to maintain our copy of
;;    `evil-collection-list' (now I can just copy it from time to time).

(defvar +evil-collection-disabled-list
  '(anaconda-mode
    buff-menu
    comint
    company
    custom
    dired
    eldoc
    elisp-mode
    ert
    free-keys
    help
    helm
    image
    ivy
    kotlin-mode
    occur
    package-menu
    ruby-mode
    simple
    slime)
  "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

;; This has to be defined here since evil-collection doesn't autoload its own.
;; It must be updated whenever evil-collection updates theirs.
(defvar evil-collection-mode-list
  `(ag
    alchemist
    anaconda-mode
    arc-mode
    bookmark
    (buff-menu "buff-menu")
    calc
    calendar
    cider
    cmake-mode
    comint
    company
    compile
    custom
    cus-theme
    daemons
    deadgrep
    debbugs
    debug
    diff-mode
    dired
    doc-view
    edebug
    ediff
    eglot
    elfeed
    elisp-mode
    elisp-refs
    emms
    epa
    ert
    eshell
    eval-sexp-fu
    evil-mc
    eww
    flycheck
    flymake
    free-keys
    geiser
    ggtags
    git-timemachine
    go-mode
    grep
    help
    guix
    helm
    ibuffer
    image
    image-dired
    image+
    imenu-list
    indium
    info
    ivy
    js2-mode
    log-view
    lsp-ui-imenu
    lua-mode
    kotlin-mode
    macrostep
    man
    magit
    magit-todos
    ,@(if (bound-and-true-p evil-collection-setup-minibuffer) '(minibuffer))
    mu4e
    mu4e-conversation
    neotree
    notmuch
    nov
    ;; occur is in replace.el which was built-in before Emacs 26.
    (occur ,(if EMACS26+ 'replace "replace"))
    outline
    p4
    (package-menu package)
    pass
    (pdf pdf-view)
    popup
    proced
    prodigy
    profiler
    python
    quickrun
    racer
    realgud
    reftex
    restclient
    rjsx-mode
    robe
    ruby-mode
    rtags
    simple
    slime
    (term term ansi-term multi-term)
    tide
    transmission
    typescript-mode
    vc-annotate
    vc-dir
    vc-git
    vdiff
    view
    vlf
    which-key
    wdired
    wgrep
    woman
    xref
    youtube-dl
    (ztree ztree-diff)))

(defun +evil-collection-init (module)
  (unless (memq (or (car-safe module) module) +evil-collection-disabled-list)
    (when doom-debug-mode
      (message "Loaded evil-collection-%s" (or (car-safe module) module)))
    (with-demoted-errors "evil-collection error: %s"
      (evil-collection-init (list module)))))


;;
;; Bootstrap

;; These modes belong to packages that Emacs always loads at startup, causing
;; evil-collection to load immediately. We avoid this by loading them after
;; evil-collection has first loaded...
(after! evil-collection
  (let (+evil-collection-disabled-list)
    (mapc #'+evil-collection-init '(comint custom help))))

;; ...or on first invokation of their associated major/minor modes.
(add-transient-hook! 'Buffer-menu-mode
  (+evil-collection-init '(buff-menu "buff-menu")))
(add-transient-hook! 'image-mode
  (+evil-collection-init 'image))
(add-transient-hook! 'emacs-lisp-mode
  (+evil-collection-init 'elisp-mode))
(add-transient-hook! 'occur-mode
  (+evil-collection-init (if EMACS26+ 'replace "replace")))

(after! helpful
  (evil-define-key* 'normal helpful-mode-map
    "o" #'ace-link-help
    "q" #'quit-window
    "]l" #'forward-button
    "[l" #'backward-button))

;; Load the rest
(dolist (mode evil-collection-mode-list)
  (dolist (req (or (cdr-safe mode) (list mode)))
    (with-eval-after-load req
      (+evil-collection-init mode))))
