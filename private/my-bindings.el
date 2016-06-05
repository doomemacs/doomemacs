;;; my-bindings.el

(eval-when-compile (require 'core-defuns))

(map! "<f9>" 'what-face
      ;; Essential
      "M-x"  'helm-M-x
      "A-x"  'helm-M-x
      "M-;"  'eval-expression
      "A-;"  'eval-expression
      ;; Tools
      "M-/"  'evil-commentary-line
      "A-/"  'evil-commentary-line
      "M-b"  'doom:build
      "A-`"  'os-switch-to-term
      "C-`"  'doom/popup-toggle
      "C-~"  'doom:repl
      ;; Text-scaling
      "M-0"  (λ! (text-scale-set 0))
      "M-="  'text-scale-increase
      "M--"  'text-scale-decrease
      ;; Simple window navigation/manipulation
      "M-t"  'doom:tab-create
      "M-T"  'doom/tab-display
      "M-w"  'doom/close-window-or-tab
      "M-W"  'delete-frame
      "M-n"  'doom/new-buffer
      "M-N"  'doom/new-frame
      "C-j"  'evil-window-down
      "C-k"  'evil-window-up
      "C-h"  'evil-window-left
      "C-l"  'evil-window-right
      "A-j"  'doom/evil-window-resize-d
      "A-k"  'doom/evil-window-resize-u
      "A-h"  'doom/evil-window-resize-l
      "A-l"  'doom/evil-window-resize-r
      ;; Temporary escape into emacs mode
      "C-<escape>" 'evil-emacs-state
      :e "C-<escape>" 'evil-normal-state
      ;; Switching tabs (workgroups)
      :m "M-1"  (λ! (doom:switch-to-tab 0))
      :m "M-2"  (λ! (doom:switch-to-tab 1))
      :m "M-3"  (λ! (doom:switch-to-tab 2))
      :m "M-4"  (λ! (doom:switch-to-tab 3))
      :m "M-5"  (λ! (doom:switch-to-tab 4))
      :m "M-6"  (λ! (doom:switch-to-tab 5))
      :m "M-7"  (λ! (doom:switch-to-tab 6))
      :m "M-8"  (λ! (doom:switch-to-tab 7))
      :m "M-9"  (λ! (doom:switch-to-tab 8))
      (:when IS-MAC
        "<M-backspace>"     'doom/backward-kill-to-bol-and-indent
        "<A-left>"          'backward-word
        "<A-right>"         'forward-word
        "A-SPC"             'just-one-space
        "M-a"               'mark-whole-buffer
        "M-c"               'evil-yank
        "M-o"               'helm-find-files
        "M-q"               'evil-quit-all
        "M-s"               'save-buffer
        "M-v"               'clipboard-yank
        "M-z"               'undo
        "M-Z"               'redo
        "C-M-f"             'doom:toggle-fullscreen
        :m  "M-j"           'doom/multi-next-line
        :m  "M-k"           'doom/multi-previous-line
        :n  "M-r"           'doom:eval-buffer
        :v  "M-r"           'doom:eval-region
        :ni "<M-f1>"        'doom:docs-lookup
        ;; Textmate-esque indent shift left/right
        :i  "M-]"           'doom/smart-indent
        :i  "M-["           'doom/dumb-dedent
        ;; Restore osx text objects
        :i  "<A-backspace>" 'evil-delete-backward-word
        :i  "<A-delete>"    'doom/delete-forward-word)

      ;;; <leader> and <localleader>
      :m ";" 'evil-ex
      (:leader
        :nv ","   'doom/helm-buffers-dwim
        :nv "."   'helm-find-files
        :nv "/"   'helm-projectile-find-file
        :nv ":"   'helm-imenu-in-all-buffers
        :nv ";"   'helm-semantic-or-imenu
        :nv "<"   'helm-buffers-list
        :v  "="   'align-regexp
        :nv ">"   'helm-projectile-find-file-in-known-projects
        :nv "]"   'helm-etags-select
        :nv "a"   'helm-projectile-find-other-file
        :n  "b"   'helm-bookmarks
        :n  "e"   'doom/flycheck-errors
        :n  "k"   'doom:docs-lookup
        :nv "l"   'doom/nlinum-toggle
        :nv "m"   'helm-recentf
        :nv "M"   'helm-projectile-recentf
        :nv "p"   'helm-show-kill-ring
        :nv "P"   'helm-projectile-switch-project
        :n  "R"   'doom/reset-theme
        :n  "s"   'yas-visit-snippet-file
        :n  "S"   'doom/yas-find-file
        ;; Quick quitting
        :nv "Q"   'evil-save-and-quit
        :nv "C-q" 'doom/kill-all-buffers-do-not-remember
        ;; Quick access to config files
        :nv "E"   'doom/helm-find-in-emacsd
        :nv "\\"  'doom/helm-find-in-dotfiles
        ;; Alternative to C-h (used as window shortcut)
        :n  "h"   'help-command
        (:prefix "d" ; <diff>
          :n "." 'doom/vcs-show-hunk
          :n "/" 'vc-diff
          :n "d" 'magit-status
          :n "D" 'git-messenger:popup-message
          :n "s" 'doom/vcs-stage-hunk
          :v "s" 'magit-stage
          :v "S" 'magit-stage
          :n "r" 'doom/vcs-revert-hunk
          :n "A" 'vc-annotate)
        (:prefix "t" ; <tmux>
          :n "." 'doom/tmux-cd-to-here
          :n "/" 'doom/tmux-cd-to-project
          :v "r" 'doom:tmux)
        (:prefix "o" ; <os>
          :n "o" 'os-open-in-default-program
          :n "r" 'os-reveal
          :n "p" 'os-reveal-project
          :n "b" 'os-open-in-browser
          :n "u" 'os-upload
          :n "U" 'os-upload-folder
          :n "t" 'os-switch-to-term
          :n "T" 'os-switch-to-term-and-cd
          (:when IS-MAC
            :n "l" 'os-send-to-launchbar
            :n "L" 'os-send-project-to-launchbar))
        (:prefix "x" ; <org>
          :n "." 'doom/org-find-file
          :n "/" 'doom/org-find-file-in-notes
          :n "e" 'doom/org-find-exported-file
          :n "x" 'doom/org))

      (:localleader
        :n "\\" 'doom/neotree
        :n "]"  'imenu-list-minor-mode
        :n "b"  'doom:build
        :n "R"  'doom:repl
        :v "R"  'doom:repl-eval
        :v "r"  'doom:eval-region
        (:prefix "r" ; <refactor/run>
          :n "e" 'emr-show-refactor-menu
          :n "r" 'doom:eval-buffer))

      ;;; Evil-esque bindings
      ;; Yank to EOL
      :n  "Y"   "y$"
      ;; Don't move cursor on indent
      :n  "="  'doom/static-reindent
      :v  "="  'evil-indent
      ;; Folding
      :n  "zr" 'doom/evil-open-folds
      :n  "zm" 'doom/evil-close-folds
      :n  "zx" 'doom/kill-real-buffer
      ;; Buffers
      :n  "ZX" 'bury-buffer
      :n  "]b" 'doom/next-real-buffer
      :n  "[b" 'doom/previous-real-buffer
      ;; Diffs
      :m  "]d" 'doom/vcs-next-hunk
      :m  "[d" 'doom/vcs-prev-hunk
      :m  "]e" 'doom/flycheck-next-error
      :m  "[e" 'doom/flycheck-previous-error
      ;; Switch tabs
      :n  "]w" 'doom:switch-to-tab-right
      :n  "[w" 'doom:switch-to-tab-left
      :m  "gt" 'doom:switch-to-tab-right
      :m  "gT" 'doom:switch-to-tab-left
      ;; Increment/decrement number under cursor
      :n  "g=" 'evil-numbers/inc-at-pt
      :n  "g-" 'evil-numbers/dec-at-pt
      ;; NOTE: Helm is too bulky for ffap (which I use for quick file navigation)
      :n  "gf" (λ! (helm-mode -1) (call-interactively 'find-file-at-point) (helm-mode 1))
      ;; Navigation
      :nv "K"  'smart-up
      :m  "gD" 'doom/find-def
      :n  "gp" 'doom/reselect-paste
      :n  "gc" 'evil-commentary
      :n  "gx" 'evil-exchange
      :n  "gr" 'doom:eval-region
      :n  "gR" 'doom:eval-buffer
      :v  "gR" 'doom:eval-region-and-replace
      :m  "g]" 'smart-right
      :m  "g[" 'smart-left
      :v  "@"  'doom/evil-macro-on-all-lines
      :n  "g@" 'doom/evil-macro-on-all-lines
      ;; Repeat in visual mode
      :v  "."  'evil-repeat
      ;; vnoremap < <gv
      :v  "<"  (λ! (evil-shift-left (region-beginning) (region-end))
                   (evil-normal-state)
                   (evil-visual-restore))
      ;; vnoremap > >gv
      :v  ">"  (λ! (evil-shift-right (region-beginning) (region-end))
                   (evil-normal-state)
                   (evil-visual-restore))
      ;; undo/redo for regions (NOTE: Buggy!)
      :nv "u"   'undo-tree-undo
      :nv "C-r" 'undo-tree-redo
      ;; paste from recent yank register (which isn't overwritten)
      :v  "C-p" "\"0p"

      (:map evil-window-map ; prefix "C-w"
        ;; Navigation
        "C-h"     'evil-window-left
        "C-j"     'evil-window-down
        "C-k"     'evil-window-up
        "C-l"     'evil-window-right
        "C-w"     'ace-window
        ;; Jump to new splits
        "s"       'doom/evil-window-split
        "v"       'doom/evil-window-vsplit
        ;; Swapping windows
        "H"       'doom/evil-window-move-l
        "J"       'doom/evil-window-move-d
        "K"       'doom/evil-window-move-u
        "L"       'doom/evil-window-move-r
        "C-S-w"   (λ! (ace-window 4))
        ;; Window undo/redo
        "u"       'doom/undo-window-change
        "C-u"     'doom/undo-window-change
        "C-r"     'doom/redo-window-change
        ;; Delete window
        "C-C"     (λ! (ace-window 16)))

      ;;; Plugins
      ;; evil-visual-star
      :v  "*"   'evil-visualstar/begin-search-forward
      :v  "#"   'evil-visualstar/begin-search-backward

      ;; evil-multiedit
      :v  "R"     'evil-multiedit-match-all
      :n  "M-C-D" 'evil-multiedit-restore
      :nv "M-d"   'evil-multiedit-match-and-next
      :nv "M-D"   'evil-multiedit-match-and-prev
      (:map evil-multiedit-state-map
        :v "RET" 'evil-multiedit-toggle-or-restrict-region)

      ;; evil-surround
      :v  "S"   'evil-surround-region
      :o  "s"   'evil-surround-edit
      :o  "S"   'evil-Surround-edit

      ;; expand-region
      :v  "v"   'er/expand-region
      :v  "V"   'er/contract-region

      ;; evil-matchit
      :m  "%"   'evilmi-jump-items

      ;; hide-show/evil-matchit
      :m  [tab] (λ! (if (ignore-errors (hs-already-hidden-p))
                        (hs-toggle-hiding)
                      (call-interactively 'evilmi-jump-items)))
      ;; rotate-text
      :n  "!"     'rotate-text

      ;; auto-yasnippet
      :i  "<C-tab>" 'aya-expand
      :nv "<C-tab>" 'aya-create

      ;; yasnippet
      (:map yas-minor-mode-map
        :i  [(tab)] 'yas-expand
        :v  [(tab)] 'doom/yas-insert-snippet)

      ;; company-mode and vim-like omni-complete
      :i "C-SPC" 'doom/company-complete
      (:prefix "C-x"
        :i "C-l"   'doom/company-whole-lines
        :i "C-k"   'doom/company-dict-or-keywords
        :i "C-f"   'company-files
        :i "C-]"   'company-tags
        :i "s"     'company-ispell
        :i "C-s"   'company-yasnippet
        :i "C-o"   'company-capf
        :i "C-n"   'company-dabbrev-code
        :i "C-p"   (λ! (let ((company-selection-wrap-around t))
                         (call-interactively 'company-dabbrev-code)
                         (company-select-previous-or-abort))))

      (:after company
        (:map company-active-map
          "C-o"        'company-search-kill-others
          "C-n"        'company-select-next
          "C-p"        'company-select-previous
          "C-h"        'company-quickhelp-manual-begin
          "C-S-h"      'company-show-doc-buffer
          "C-S-s"      'company-search-candidates
          "C-s"        'company-filter-candidates
          "C-SPC"      'company-complete-common-or-cycle
          [tab]        'doom/company-complete-common-or-complete-full
          "<backtab>"  'company-select-previous
          [escape]     (λ! (company-abort) (evil-normal-state 1))
          "<C-return>" 'helm-company)
        (:map company-search-map
          "C-n"        'company-search-repeat-forward
          "C-p"        'company-search-repeat-backward
          [escape]     'company-search-abort))

      ;; help-mode
      (:after help-mode
        (:map help-map
          "e" 'doom/popup-messages)
        (:map help-mode-map
          :n "]]"  'help-go-forward
          :n "[["  'help-go-back
          :n "o"   'ace-link-help))

      ;;; Insert mode hacks
      ;; Textmate-esque newlines
      :i "<backspace>"   'delete-backward-char
      :i "<M-backspace>" 'doom/backward-kill-to-bol-and-indent
      :i "<C-return>"    'evil-ret-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" 'backward-word
      :i "C-f" 'forward-word
      ;; escape from insert mode (more responsive than using key-chord-define)
      :irv "C-g" 'evil-normal-state)

;; Common unicode characters
(map! :i "A-o" (λ! (insert "ø"))
      :i "A-O" (λ! (insert "Ø"))

      :i "A--" (λ! (insert "–"))
      :i "A-_" (λ! (insert "—")))

(provide 'my-bindings)
;;; my-bindings.el ends here
