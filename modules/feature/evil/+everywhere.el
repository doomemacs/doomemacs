;;; feature/evil/+everywhere.el -*- lexical-binding: t; -*-

;; Here we *truly* lazy-load evil-collection's modules by ensuring its modules
;; do not load at startup (some of them, like buff-menu, help or elisp-mode are
;; loaded immediately, causing evil-collection to be pulled in).
;;
;; We load evil-collection ourselves for three reasons:
;;
;; 1. To truly lazy load it. Some of its modules, like the elisp-mode and
;;    buff-menu ones are loaded immediately, because Emacs loads them
;;    immediately.
;; 2. This ensures a predictable load order, versus lazy loading using :defer or
;;    :after-call. This means users can use (after! org ...) and be sure that
;;    their changes will override evil-collection's.
;; 3. I don't completely agree with all of evil-collection's design choices.
;;    Sometimes, I disagree with entire modules. Other times it's just a couple
;;    keybinds. I'd rather do all this integration work internally, rather than
;;    delegate it to another package that I cannot control or predict.
;; 4. Adds `+evil-collection-disabled-list', to make it easier for users to
;;    disable modules.

(defvar +evil-collection-disabled-list ()
  "A list of `evil-collection' modules to ignore. See the definition of this
variable for an explanation of the defaults (in comments). See
`evil-collection-mode-list' for a list of available options.")

(defvar evil-collection-mode-list
  `(ace-jump-mode
    ag
    alchemist
    ;; anaconda-mode
    arc-mode
    avy
    bookmark
    ;; (buff-menu "buff-menu")
    calc
    calendar
    cider
    cmake-mode
    ;; comint
    ;; company
    compile
    ;; custom
    cus-theme
    daemons
    debbugs
    debug
    diff-mode
    ;; dired
    doc-view
    edebug
    ediff
    ;; eldoc
    elfeed
    ;; elisp-mode
    elisp-refs
    emms
    epa
    ;; ert
    eshell
    eval-sexp-fu
    etags-select
    eww
    flycheck
    ;; free-keys
    geiser
    ggtags
    git-timemachine
    go-mode
    ;; help
    guix
    ;; helm
    ibuffer
    ;; image
    image+
    indium
    info
    ;; ivy
    js2-mode
    log-view
    lsp-ui-imenu
    lua-mode
    ;; kotlin-mode
    macrostep
    man
    magit
    mu4e
    mu4e-conversation
    neotree
    notmuch
    nov
    ;; occur is in replace.el which was built-in before Emacs 26.
    ;; (occur ,(if EMACS26+ 'replace "replace"))
    outline
    p4
    ;; (package-menu package)
    paren
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
    ;; ruby-mode
    rtags
    ;; simple
    ;; slime
    (term term ansi-term multi-term)
    tide
    transmission
    typescript-mode
    vc-annotate
    vdiff
    view
    vlf
    which-key
    wdired
    wgrep
    woman
    xref
    (ztree ztree-diff)))

(defun +evil-collection-init (module)
  (unless (memq (or (car-safe module) module) +evil-collection-disabled-list)
    (when doom-debug-mode
      (message "Loaded evil-collection-%s" (or (car-safe module) module)))
    (evil-collection-init (list module))))


;;
;; Bootstrap
;;

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
;; evil-collection to load immediately. By tacking it on to the modes
;; themselves, rather than the package being loaded, we manage to truly lazy
;; load evil-collection.
(add-transient-hook! 'Buffer-menu-mode
  (+evil-collection-init '(buff-menu "buff-menu")))
(add-transient-hook! 'image-mode
  (+evil-collection-init 'image))
(add-transient-hook! 'emacs-lisp-mode
  (+evil-collection-init 'elisp-mode))
(add-transient-hook! 'occur-mode
  (+evil-collection-init (if EMACS26+ 'replace "replace")))

;; Let 'er rip!
(dolist (mode evil-collection-mode-list)
  (dolist (req (or (cdr-safe mode) (list mode)))
    (with-eval-after-load req
      (+evil-collection-init mode))))
