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

(defun +evil-collection-init (module)
  (unless (memq (or (car-safe module) module) +evil-collection-disabled-list)
    (when doom-debug-mode
      (message "Loaded evil-collection-%s" (or (car-safe module) module)))
    (with-demoted-errors "evil-collection error: %s"
      (evil-collection-init (list module)))))


;;
;; Bootstrap

(after! eldoc
  (eldoc-add-command-completions "evil-window-"))

(after! comint
  (evil-define-key* 'normal comint-mode-map
    (kbd "C-d") #'evil-scroll-down
    (kbd "C-n") #'comint-next-input
    (kbd "C-p") #'comint-previous-input
    (kbd "gj") #'comint-next-input
    (kbd "gk") #'comint-previous-input
    (kbd "]") #'comint-next-input
    (kbd "[") #'comint-previous-input)
  (evil-define-key* 'insert comint-mode-map
    (kbd "<up>") #'comint-previous-input
    (kbd "<down>") #'comint-next-input))

(after! cus-edit
  (evil-set-initial-state 'Custom-mode 'normal)
  (evil-define-key* 'motion custom-mode-map
    (kbd "<tab>") #'widget-forward
    (kbd "S-<tab>") #'widget-backward
    (kbd "<backtab>") #'widget-backward
    (kbd "]") #'widget-forward
    (kbd "[") #'widget-backward
    (kbd "C-n") #'widget-forward
    (kbd "C-p") #'widget-backward
    "gj" #'widget-forward
    "gk" #'widget-backward)
  (evil-define-key* 'normal custom-mode-map
    (kbd "<return>") #'Custom-newline
    (kbd "C-o") #'Custom-goto-parent
    "^" #'Custom-goto-parent
    "<" #'Custom-goto-parent
    ;; quit
    "q" #'Custom-buffer-done
    "ZQ" #'evil-quit
    "ZZ" #'Custom-buffer-done))

(after! help-mode
  (evil-set-initial-state 'help-mode 'normal)
  (evil-define-key* 'normal help-mode-map
    ;; motion
    (kbd "SPC") #'scroll-up-command
    (kbd "S-SPC") #'scroll-down-command
    (kbd "C-f") #'scroll-up-command
    (kbd "C-b") #'scroll-down-command
    (kbd "<tab>") #'forward-button
    (kbd "<backtab>") #'backward-button
    (kbd "C-o") #'help-go-back
    (kbd "C-i") #'help-go-forward
    ;; TODO: Enable more help-go-* bindings?
    ;; "gj" #'help-go-forward
    ;; "gk" #'help-go-back
    ;; "\C-j" #'help-go-forward
    ;; "\C-k" #'help-go-back
    ;; The following bindings don't do what they are supposed to. "go" should
    ;; open in the same window and "gO" should open in a different one.
    "go" #'push-button
    "gO" #'push-button
    "g?" #'describe-mode
    "gr" #'revert-buffer
    "<" #'help-go-back
    ">" #'help-go-forward
    "r" #'help-follow
    ;; quit
    "q" #'quit-window
    "ZQ" #'evil-quit
    "ZZ" #'quit-window))

;; These modes belong to packages that Emacs always loads at startup, causing
;; evil-collection to load immediately. We avoid this by loading them on first
;; invokation of their associated major/minor modes.
(add-transient-hook! 'Buffer-menu-mode
  (+evil-collection-init '(buff-menu "buff-menu")))
(add-transient-hook! 'image-mode
  (+evil-collection-init 'image))
(add-transient-hook! 'emacs-lisp-mode
  (+evil-collection-init 'elisp-mode))
(add-transient-hook! 'occur-mode
  (+evil-collection-init (if EMACS26+ 'replace "replace")))


;;
;; Let 'er rip!

(defvar evil-collection-setup-minibuffer nil)
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
    ,@(when evil-collection-setup-minibuffer '(minibuffer))
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

(dolist (mode evil-collection-mode-list)
  (dolist (req (or (cdr-safe mode) (list mode)))
    (with-eval-after-load req
      (+evil-collection-init mode))))
