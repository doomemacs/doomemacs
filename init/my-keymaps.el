(provide 'my-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymaps                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-:") 'eval-expression)
(global-set-key (kbd "C-j") "5j")
(global-set-key (kbd "C-k") "5k")

(when is-mac
  (when window-system
    (global-set-key (kbd "s-=")     'text-scale-increase)
    (global-set-key (kbd "s--")     'text-scale-decrease)
    (global-set-key (kbd "s-w")     'evil-window-delete)
    (global-set-key (kbd "s-/")     'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "s-<f12>") 'toggle-frame-fullscreen)

    ;; Faster scrolling
    (mapc (lambda(map)
            (evil-define-key map my-mode-map (kbd "s-j") "5j")
            (evil-define-key map my-mode-map (kbd "s-k") "5k"))
          '(emacs normal visual))

    (nmap!  (kbd "s-t")   'projectile-find-file
            (kbd "s-p")   'projectile-switch-project
            (kbd "s-m")   'my:ex:mru
            (kbd "s-M")   'projectile-recentf
            (kbd "s-o")   'ido-find-file
            (kbd "s-d")   'dash-at-point
            (kbd "s-b")   'my:ex:build)

    (nvmap! (kbd "s-r")   ",r"
            (kbd "s-R")   ",R")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymaps                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remap ; to : - SPC and shift-SPC replace ; and , - have to use
;; define-key instead of n/vmap for this one to register.
(-nvmap ";" 'evil-ex)
(-nvmap "X" 'evil-exchange)

;;;; <Leader> ;;;;;;;;;;;;;;;;;;;;;;;;;;
(nmap!  ",r"   'my:run-code-buffer
        ",R"   'my:switch-to-repl
        ",b"   'my:build
        ",a"   'projectile-find-other-file
        ",e"   'ido-find-file
        ",E"   'my:ex:init-files
        ",m"   'my:ex:mru                ; recent GLOBAL files
        ",M"   'projectile-recentf       ; recent PROJECT files
        ",p"   'projectile-switch-project
        ",g"   'git-gutter+-show-hunk
        ",;"   'helm-imenu
        ",:"   'my:goto-symbol
        ",,"   'ido-switch-buffer
        ",."   'projectile-find-file)

(vmap!  ",r"   'my:run-code-region
        ",R"   'my:send-region-to-repl)

(nvmap! ",="   'align-regexp)

;;;; <localleader> ;;;;;;;;;;;;;;;;;;;;;
(-nmap  "\\"   'evil-execute-in-god-state)
(gmap!  ":"    'linum-mode
        "\\"   'neotree-show
        "|"    'neotree-hide

        "oo"   'my:send-dir-to-finder
        "ou"   'my:send-to-transmit
        "ol"   'my:send-to-launchbar
        "oL"   'my:send-dir-to-launchbar

        ;; tmux: cd (default-directory)
        "ot"   (λ (my:ex:tmux-chdir nil t))
        ;; tmux: cd [project root]
        "oT"   'my:ex:tmux-chdir)

;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;
(nvmap! "gc"   'evil-ace-jump-char-mode
        "gw"   'evil-ace-jump-word-mode  ; overwrites evil-fill
        "gl"   'evil-ace-jump-line-mode

        "]\\"  'er/expand-region
        "[\\"  'er/contract-region

        "]g"   'git-gutter+-stage-hunks
        "[g"   'git-gutter+-revert-hunks)

(nmap!  ;; Moving rows rather than lines (in case of wrapping)
        "j"    'evil-next-visual-line
        "k"    'evil-previous-visual-line

        ;; behave  like D and C; yank to end of line
        "Y"    (λ (evil-yank (point) (point-at-eol)))

        "zz"   'kill-this-buffer
        "zx"   'bury-buffer

        "]b"   'next-buffer
        "[b"   'previous-buffer
        "]e"   'next-error
        "[e"   'previous-error
        "]h"   'git-gutter+-next-hunk
        "[h"   'git-gutter+-previous-hunk

        ;; winner-mode: window layout undo/redo (see init-core.el)
        (kbd "C-w u")     'winner-undo
        (kbd "C-w C-r")   'winner-redo

        ;; Increment/decrement number under cursor
        (kbd "C--")       'evil-numbers/inc-at-pt
        (kbd "C-+")       'evil-numbers/dec-at-pt)

(vmap!  ;; vnoremap < <gv
        "<"    (λ (evil-shift-left (region-beginning) (region-end))
                    (evil-normal-state)
                    (evil-visual-restore))
        ;; vnoremap > >gv
        ">"    (λ (evil-shift-right (region-beginning) (region-end))
                    (evil-normal-state)
                    (evil-visual-restore)))

(imap!  ;; Join lines from insert mode
        (kbd "<M-kp-delete>") 'evil-join

        ;; Newline magic
        (kbd "<C-return>")    'evil-ret-and-indent
        (kbd "<M-return>")    (kbd "<return> DEL") ; newline and dedent

        ;; Textmate-esque indent shift left/right
        (kbd "s-[")           (kbd "C-o m l C-o I DEL C-o ` l")
        (kbd "s-]")           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
        (kbd "<backtab>")     (kbd "s-["))

(emap!  ;; Preserve buffer-movement in emacs mode
        "\C-j"        'evil-next-line
        "\C-k"        'evil-previous-line

        (kbd "C-w h") 'evil-window-left
        (kbd "C-w l") 'evil-window-right
        (kbd "C-w j") 'evil-window-down
        (kbd "C-w k") 'evil-window-up)

;; Rotate-text (see elisp/rotate-text.el)
(nmap!  "!" 'rotate-word-at-point)
(vmap!  "!" 'rotate-region)

;; Easy escape from insert mode
(ichmap "jj" 'evil-normal-state)

;; Enable TAB to do matchit
(nmap evil-matchit-mode-map (kbd "TAB") 'evilmi-jump-items)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin/mode keymaps                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Real go-to-definition for elisp
(nmap emacs-lisp-mode-map "gd"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function func)))))

(after ag
       (defmap ag-mode-map
               [escape] 'ag-kill-buffers
               "h"      nil))

(after auto-complete
       (imap ac-mode-map
             (kbd "C-x C-f")   'ac-complete-filename
             (kbd "C-SPC")     'auto-complete)

       (defmap ac-completing-map
             (kbd "<tab>") 'ac-complete
             (kbd "C-n") 'ac-next
             (kbd "C-p") 'ac-previous
             (kbd "<F1>") 'ac-quick-help
             (kbd "ESC") 'ac-stop
             (kbd "RET") 'ac-complete))

(after emmet-mode
       (imap emmet-mode-keymap
             (kbd "s-e") 'emmet-expand-yas
             (kbd "s-E") 'emmet-expand-line))

(after js-mode
       (imap js-mode-map [remap auto-complete] 'tern-ac-complete))

(after markdown-mode
       (let ((map markdown-mode-map))
         (nvmap map
                (kbd ",i") 'markdown-insert-image
                (kbd ",l") 'markdown-insert-link
                (kbd ",L") 'markdown-insert-reference-link-dwim)

         (nmap map
               "[p" 'markdown-promote
               "]p" 'markdown-demote)

         (imap map (kbd "M--") 'markdown-insert-hr)

         (defmap map
           (kbd "<backspace>")  nil
           (kbd "<M-left>")     nil
           (kbd "<M-right>")    nil

           (kbd "s-*") 'markdown-insert-list-item
           (kbd "s-b") 'markdown-insert-bold
           (kbd "s-i") 'markdown-insert-italic
           (kbd "s-`") 'markdown-insert-del)))

(after multiple-cursors
       (imap mc/keymap
             (kbd "s-'")   'mc/mark-next-like-this
             (kbd "s-\"")  'mc/mark-previous-like-this
             (kbd "C-s-'") 'mc/mark-all-like-this)

       (vmap mc/keymap
             (kbd "s-'")   'mc/mark-next-like-this
             (kbd "s-\"")  'mc/mark-previous-like-this
             (kbd "C-s-'") 'mc/mark-all-like-this))

(after nose
       (nmap nose-mode-map
             ",tr" 'nosetests-again
             ",ta" 'nosetests-all
             ",ts" 'nosetests-one
             ",tv" 'nosetests-module
             ",tA" 'nosetests-pdb-all
             ",tO" 'nosetests-pdb-one
             ",tV" 'nosetests-pdb-module))

(after org
       (define-key org-mode-map (kbd "RET") nil)
       (define-key org-mode-map (kbd "C-j") nil)
       (define-key org-mode-map (kbd "C-k") nil)

       ;; Formatting shortcuts
       (imap evil-org-mode-map
             (kbd "s-b") (λ (my/org-surround "*"))     ; bold
             (kbd "s-u") (λ (my/org-surround "_"))     ; underline
             (kbd "s-i") (λ (my/org-surround "/"))     ; italics
             (kbd "s-`") (λ (my/org-surround "+"))     ; strikethrough
             )

       (nvmap evil-org-mode-map
              ",l" 'org-insert-link)

       (vmap evil-org-mode-map
             (kbd "s-b") "s*"          ; bold
             (kbd "s-i") "s/")         ; italics

       (nmap evil-org-mode-map
             ",d" 'org-time-stamp
             ",D" 'org-time-stamp-inactive
             ",s" 'org-schedule
             ",a" 'org-attach
             ",A" 'org-attach-open
             ",t" 'org-todo
             ",T" 'org-show-todo-tree
             ",/" 'org-match-sparse-tree
             ",?" 'org-tags-view
             ",+" 'org-align-all-tags
             ",r" 'org-refile
             "gh" 'outline-up-heading
             "gj" 'org-forward-heading-same-level
             "gk" 'org-backward-heading-same-level
             "gl" 'outline-next-visible-heading
             "go" 'org-open-at-point
             "ga" 'org-agenda
             "H" 'org-beginning-of-line
             "L" 'org-end-of-line
             "$" 'org-end-of-line
             "^" 'org-beginning-of-line
             "<" 'org-metaleft
             ">" 'org-metaright
             "-" 'org-cycle-list-bullet
             (kbd ", SPC") 'org-archive-subtree
             (kbd "<S-s-return>") (λ (evil-move-beginning-of-line) (org-insert-heading) (evil-insert-state))
             (kbd "<s-return>") (λ (org-insert-heading-after-current) (evil-insert-state))
             (kbd "RET") (λ (if (org-entry-is-todo-p) (org-todo 'done)))
             (kbd "TAB") 'org-cycle))

(after ruby-mode
       (nmap ruby-mode-map "gd" 'rsense-jump-to-definition))

(after rspec-mode
       (nmap rspec-mode-verifiable-keymap
             ",tr" 'rspec-rerun
             ",ta" 'rspec-verify-all
             ",ts" 'rspec-verify-single
             ",tv" 'rspec-verify)
       (nmap rspec-dired-mode-keymap
             ",tv" 'rspec-dired-verify
             ",ts" 'rspec-dired-verify-single
             ",ta" 'rspec-verify-all
             ",tr" 'rspec-rerun))

(after web-mode
       (defmap web-mode-map (kbd "s-/") 'web-mode-comment-or-uncomment)

       (nvmap web-mode-map
              "]a" 'web-mode-attribute-next
              "]t" 'web-mode-tag-next
              "[t" 'web-mode-tag-previous
              "]T" 'web-mode-element-child
              "[T" 'web-mode-element-parent)

       (nmap web-mode-map
             "zf" 'web-mode-fold-or-unfold
             ",t" 'web-mode-element-rename))

(after re-builder
       (nmap reb-mode-map
             ",r" 'reb-enter-subexp-mode
             ",b" 'reb-copy
             ",i" 'reb-change-syntax
             "\C-n" 'reb-next-match
             "\C-p" 'reb-prev-match))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ex Commands                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(exmap "msg"         'my:ex:msg-buffer)
(exmap "recompile"   'my:ex:byte-compile-all)
(exmap "n[otes]"     'my:ex:notes)
(exmap "ini"         'my:ex:init-files)
(exmap "snip[pets]"  'my:ex:snippets)
(exmap "mru"         'my:ex:mru)

(exmap "retab"       'my:ex:retab)
(exmap "ag"          'my:ex:ag-search)
(exmap "agr"         'my:ex:ag-regex-search)
(exmap "x"           'my:ex:scratch-buffer)
(exmap "X"           'my:ex:org-capture)
(exmap "a"           'projectile-find-other-file)
(exmap "bx"          'my:ex:kill-buffers)
(exmap "tcd"         'my:ex:tmux-chdir)
(exmap "t[mux]"      'my:ex:tmux-send)
(exmap "build"       'my:ex:build)
(exmap "re[gex]"     're-builder)
