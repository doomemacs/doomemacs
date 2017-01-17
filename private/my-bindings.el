;;; my-bindings.el --- description

(defmacro @find-file-in (path &optional project-p)
  "Returns a interactive function for searching files"
  `(lambda () (interactive)
     (let ((default-directory ,path))
       ,(if project-p
             '(call-interactively 'projectile-find-file)
           '(counsel-find-file)))))

(map! [f9]   'what-face
      ;; Essential
      ;; "M-x"  'counsel-M-x
      ;; "A-x"  'counsel-M-x
      "M-;"  'eval-expression
      "A-;"  'eval-expression
      ;; Tools
      "M-/"  'evil-commentary-line
      "A-/"  'evil-commentary-line
      "M-b"  'doom:build
      "C-`"  'doom/popup-last-buffer
      "C-~"  (λ! (doom:eshell t))
      ;; Text-scaling
      "M-0"  (λ! (text-scale-set 0))
      "M-="  'text-scale-increase
      "M--"  'text-scale-decrease
      ;; Simple window navigation/manipulation
      "M-t"  'doom:workspace-new
      "M-T"  'doom/workspace-display
      "M-w"  'doom/workspace-or-window-close
      "M-W"  'doom/workspace-close-frame
      "M-n"  'evil-buffer-new
      "M-N"  'doom/workspace-new-frame
      "C-j"  'evil-window-down
      "C-k"  'evil-window-up
      "C-h"  'evil-window-left
      "C-l"  'evil-window-right
      ;; Temporary escape into emacs mode
      [C-escape]    'evil-emacs-state
      :e [C-escape] 'evil-normal-state
      ;; Basic escape keys for emacs mode
      :e "C-h"  'evil-window-left
      :e "C-j"  'evil-window-down
      :e "C-k"  'evil-window-up
      :e "C-l"  'evil-window-right
      ;; Switching tabs (workgroups)
      "M-1"  (λ! (doom/workspace-activate-at 0))
      "M-2"  (λ! (doom/workspace-activate-at 1))
      "M-3"  (λ! (doom/workspace-activate-at 2))
      "M-4"  (λ! (doom/workspace-activate-at 3))
      "M-5"  (λ! (doom/workspace-activate-at 4))
      "M-6"  (λ! (doom/workspace-activate-at 5))
      "M-7"  (λ! (doom/workspace-activate-at 6))
      "M-8"  (λ! (doom/workspace-activate-at 7))
      "M-9"  (λ! (doom/workspace-activate-at 8))

      [M-backspace]       'doom/backward-kill-to-bol-and-indent
      [A-left]            'backward-word
      [A-right]           'forward-word
      "A-SPC"             'just-one-space
      "M-a"               'mark-whole-buffer
      "M-c"               'evil-yank
      "M-q"               'save-buffers-kill-emacs
      "M-s"               'save-buffer
      "M-v"               'clipboard-yank
      "M-z"               'undo-tree-undo
      "M-Z"               'undo-tree-redo
      "C-M-f"             'doom-ui-toggle-fullscreen
      :m  "A-j"           'doom:multi-next-line
      :m  "A-k"           'doom:multi-previous-line
      :n  "M-r"           'doom:eval-buffer
      :v  "M-r"           'doom:eval-region
      :ni [M-f1]          'doom:docs-lookup

      ;;; <leader> and <localleader>
      :m ";" 'evil-ex
      ;; (:leader
      ;;   :nv ","   'projectile-switch-to-buffer
      ;;   :nv "<"   'ivy-switch-buffer
      ;;   :nv "."   (@find-file-in default-directory)
      ;;   :nv "/"   'counsel-projectile-find-file
      ;;   :n  ":"   'ivy-imenu-anywhere
      ;;   :nv ";"   'counsel-imenu
      ;;   :v  "="   'align-regexp
      ;;   :nv "a"   'projectile-find-other-file
      ;;   :n  "b"   'counsel-bookmark
      ;;   :n  "B"   'bookmark-delete
      ;;   :n  "e"   'doom/flycheck-errors
      ;;   :n  "k"   'doom:docs-lookup
      ;;   :nv "l"   'doom/nlinum-toggle
      ;;   :nv "m"   'counsel-recentf
      ;;   :nv "M"   'projectile-recentf
      ;;   :nv "p"   'counsel-yank-pop
      ;;   :nv "P"   'counsel-projectile-switch-project
      ;;   :n  "r"   'emr-show-refactor-menu
      ;;   :n  "R"   'doom-ui-reset-theme
      ;;   :n  "s"   'yas-visit-snippet-file
      ;;   :n  "S"   'doom/yas-find-file
      ;;   ;; Quick quitting
      ;;   :nv "Q"   'evil-save-and-quit
      ;;   :nv "C-q" 'doom/kill-workgroup-and-quit
      ;;   ;; Quick access to config files
      ;;   :nv "E"   (@find-file-in doom-emacs-dir t)
      ;;   :nv "\\"  (@find-file-in (f-expand ".dotfiles" "~") t)
      ;;   ;; Alternative to C-h (used as window shortcut)
      ;;   :n  "h"   'help-command
      ;;   (:prefix "d" ; <diff>
      ;;     :n "." 'git-gutter:popup-hunk
      ;;     :n "/" 'vc-diff
      ;;     :n "d" 'magit-status
      ;;     :n "D" 'git-messenger:popup-message
      ;;     :n "s" 'git-gutter:stage-hunk
      ;;     :v "s" 'magit-stage
      ;;     :v "S" 'magit-stage
      ;;     :n "r" 'git-gutter:revert-hunk
      ;;     :n "A" 'vc-annotate)
      ;;   (:prefix "t" ; <tmux>
      ;;     :n "." 'doom/tmux-cd-to-here
      ;;     :n "/" 'doom/tmux-cd-to-project
      ;;     :v "r" 'doom:tmux)
      ;;   (:prefix "o" ; <os>
      ;;     :n "o" 'os-open-in-default-program
      ;;     :n "r" 'os-reveal
      ;;     :n "p" 'os-reveal-project
      ;;     :n "b" 'os-open-in-browser
      ;;     :n "u" 'os-upload
      ;;     :n "U" 'os-upload-folder
      ;;     :n "t" 'os-switch-to-term
      ;;     :n "T" 'os-switch-to-term-and-cd
      ;;     (:when IS-MAC
      ;;       :n "l" 'os-send-to-launchbar
      ;;       :n "L" 'os-send-project-to-launchbar))
      ;;   (:prefix "x" ; <org>
      ;;     :n "x" 'doom/org
      ;;     :n "." (@find-file-in org-directory)
      ;;     :n "/" (@find-file-in org-directory t)
      ;;     :n "e" (@find-file-in org-export-directory)))

      ;; (:localleader
      ;;   :n "\\" 'doom/neotree
      ;;   :n "b"  'doom:build
      ;;   :n "R"  'doom:repl
      ;;   :v "R"  'doom:repl-eval
      ;;   ;;; conventions
      ;;   ;; ;    appends a terminator to line
      ;;   ;; tt   rerun last test
      ;;   ;; tr   run current module
      ;;   ;; ta   run all modules
      ;;   ;; ts   run single test
      ;;   ;; anything else is fair game
      ;;   )

      ;; ;;; Evil-esque bindings
      ;; ;; Yank to EOL
      ;; :n  "Y"   "y$"
      ;; ;; Don't move cursor on indent
      ;; :n  "="  'doom/static-reindent
      ;; :v  "="  'evil-indent
      ;; ;; Folding
      ;; :n  "zr" 'doom:evil-open-folds-recursively
      ;; :n  "zm" 'doom:evil-close-folds-recursively
      ;; :n  "zx" 'doom/kill-real-buffer
      ;; ;; Buffers
      ;; :n  "ZX" 'bury-buffer
      ;; :n  "]b" 'doom/next-real-buffer
      ;; :n  "[b" 'doom/previous-real-buffer
      ;; ;; Diffs
      ;; :m  "]d" 'git-gutter:next-hunk
      ;; :m  "[d" 'git-gutter:previous-hunk
      ;; :m  "]e" 'doom/flycheck-next-error
      ;; :m  "[e" 'doom/flycheck-previous-error
      ;; ;; Switch tabs
      ;; :n  "]w" 'doom:workspace-switch-right
      ;; :n  "[w" 'doom:workspace-switch-left
      ;; :m  "gt" 'doom:workspace-switch-right
      ;; :m  "gT" 'doom:workspace-switch-left
      ;; ;; Increment/decrement number under cursor
      ;; :n  "g=" 'evil-numbers/inc-at-pt
      ;; :n  "g-" 'evil-numbers/dec-at-pt
      ;; :n  "gf" 'find-file-at-point
      ;; ;; Navigation
      ;; :nv "gK" 'smart-up
      ;; :m  "gD" 'doom/find-def
      ;; :n  "gp" 'doom/editor-reselect-paste
      ;; :n  "gc" 'evil-commentary
      ;; :n  "gx" 'evil-exchange
      ;; :n  "gr" 'doom:eval-region
      ;; :n  "gR" 'doom:eval-buffer
      ;; :v  "gR" 'doom:eval-region-and-replace
      ;; :m  "g]" 'smart-forward
      ;; :m  "g[" 'smart-backward
      ;; :v  "@"  'doom:evil-macro-on-all-lines
      ;; :n  "g@" 'doom:evil-macro-on-all-lines
      ;; ;; Repeat in visual mode
      ;; :v  "."  'evil-repeat
      ;; ;; vnoremap < <gv
      ;; :v  "<"  (λ! (evil-shift-left (region-beginning) (region-end))
      ;;              (evil-normal-state)
      ;;              (evil-visual-restore))
      ;; ;; vnoremap > >gv
      ;; :v  ">"  (λ! (evil-shift-right (region-beginning) (region-end))
      ;;              (evil-normal-state)
      ;;              (evil-visual-restore))
      ;; ;; undo/redo for regions (NOTE: Buggy!)
      ;; :nv "u"   'undo-tree-undo
      ;; :nv "C-r" 'undo-tree-redo
      ;; ;; paste from recent yank register (which isn't overwritten)
      ;; :v  "C-p" "\"0p"

      ;; (:map evil-window-map ; prefix "C-w"
      ;;   ;; Navigation
      ;;   "C-h"     'evil-window-left
      ;;   "C-j"     'evil-window-down
      ;;   "C-k"     'evil-window-up
      ;;   "C-l"     'evil-window-right
      ;;   "C-w"     'ace-window
      ;;   ;; Swapping windows
      ;;   "H"       (λ! (doom-evil-window-move 'left))
      ;;   "J"       (λ! (doom-evil-window-move 'down))
      ;;   "K"       (λ! (doom-evil-window-move 'up))
      ;;   "L"       (λ! (doom-evil-window-move 'right))
      ;;   "C-S-w"   (λ! (ace-window 4))
      ;;   ;; Window undo/redo
      ;;   "u"       'doom/workspace-undo
      ;;   "C-u"     'doom/workspace-undo
      ;;   "C-r"     'doom/workspace-redo
      ;;   "o"       'doom/evil-window-zoom
      ;;   ;; Delete window
      ;;   "C-C"     (λ! (ace-window 16)))

      ;; ;;; Plugins
      ;; ;; evil-visual-star
      ;; :v  "*"   'evil-visualstar/begin-search-forward
      ;; :v  "#"   'evil-visualstar/begin-search-backward

      ;; ;; evil-multiedit
      ;; :v  "R"     'evil-multiedit-match-all
      ;; :n  "M-C-D" 'evil-multiedit-restore
      ;; :n  "M-d"   'evil-multiedit-match-symbol-and-next
      ;; :n  "M-D"   'evil-multiedit-match-symbol-and-prev
      ;; :v  "M-d"   'evil-multiedit-match-and-next
      ;; :v  "M-D"   'evil-multiedit-match-and-prev

      ;; ;; evil-surround
      ;; :v  "S"   'evil-surround-region
      ;; :o  "s"   'evil-surround-edit
      ;; :o  "S"   'evil-Surround-edit

      ;; ;; expand-region
      ;; :v  "v"   'er/expand-region
      ;; :v  "V"   'er/contract-region

      ;; ;; evil-matchit
      ;; :m  "%"   'evilmi-jump-items
      ;; ;; hide-show/evil-matchit
      ;; :m  "<tab>" 'doom/evil-matchit-or-toggle-fold
      ;; ;; rotate-text
      ;; :n  "!"     'rotate-text

      ;; ;; help-mode
      ;; (:after help-mode
      ;;   (:map help-map
      ;;     "e" 'doom/popup-messages)
      ;;   (:map help-mode-map
      ;;     :n "]]"  'help-go-forward
      ;;     :n "[["  'help-go-back
      ;;     :n "o"   'ace-link-help))

      ;; ;;; Insert mode hacks
      ;; ;; Textmate-esque newlines
      ;; :i [backspace]    'delete-backward-char
      ;; :i [M-backspace]  'doom/backward-kill-to-bol-and-indent
      ;; :i [C-return]     (λ! (evil-open-below 1))
      ;; ;; Emacsien motions for insert mode
      ;; :i "C-b" 'backward-word
      ;; :i "C-f" 'forward-word
      ;; ;; escape from insert mode (more responsive than using key-chord-define)
      ;; :irv "C-g" 'evil-normal-state
      )

(provide 'my-bindings)
