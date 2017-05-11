;;; private/hlissner/+bindings.el

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta)

(defmacro find-file-in! (path &optional project-p)
  "Returns an interactive function for searching files"
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(command-remapping
           (if project-p 'projectile-find-file) 'find-file)))))

(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag)

(map!
 ;; Essential
 "M-x"    #'execute-extended-command
 "A-x"    #'execute-extended-command
 "M-;"    #'eval-expression
 "A-;"    #'eval-expression
 ;; Tools
 [f9]     #'doom/what-face
 [f10]    #'doom/blink-cursor
 "C-`"    #'doom/popup-toggle
 ;; Text-scaling
 "M-0"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease
 ;; Simple window navigation/manipulation
 "M-t"    #'+workspace/new
 "M-T"    #'+workspace/display
 "M-w"    #'delete-window
 "M-W"    #'delete-frame
 "M-n"    #'evil-buffer-new
 "M-N"    #'make-frame
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-h"    #'evil-window-left
 "C-l"    #'evil-window-right
 ;; Basic escape keys for emacs mode
 :e "C-h" #'evil-window-left
 :e "C-j" #'evil-window-down
 :e "C-k" #'evil-window-up
 :e "C-l" #'evil-window-right
 ;; Temporary escape into emacs mode
 :e [C-escape] #'evil-normal-state
 :n [C-escape] #'evil-emacs-state
 ;; Switching tabs (workspaces)
 "M-1"  (λ! (+workspace/switch-to 0))
 "M-2"  (λ! (+workspace/switch-to 1))
 "M-3"  (λ! (+workspace/switch-to 2))
 "M-4"  (λ! (+workspace/switch-to 3))
 "M-5"  (λ! (+workspace/switch-to 4))
 "M-6"  (λ! (+workspace/switch-to 5))
 "M-7"  (λ! (+workspace/switch-to 6))
 "M-8"  (λ! (+workspace/switch-to 7))
 "M-9"  (λ! (+workspace/switch-to 8))
 "M-0"  #'+workspace/switch-to-last

 "M-r"  #'+eval/buffer
 "M-b"  #'+eval/build

 [M-backspace]  #'doom/backward-kill-to-bol-and-indent
 "M-a"          #'mark-whole-buffer
 "M-c"          #'evil-yank
 "M-q"          #'save-buffers-kill-emacs
 "M-s"          #'save-buffer
 "M-v"          #'clipboard-yank
 "M-f"          #'+ivy:swiper
 "C-M-f"        #'doom/toggle-fullscreen
 :m "A-j"       #'+hlissner:multi-next-line
 :m "A-k"       #'+hlissner:multi-previous-line

 ;;; <leader> and <localleader>
 :m ";" 'evil-ex
 (:leader
   ;; common
   :desc "Switch project buffer"    :n ","  #'persp-switch-to-buffer
   :desc "Switch buffer"            :n "<"  #'switch-to-buffer
   :desc "Browse files"             :n "."  #'find-file
   :desc "Find file from here"      :n ">"  #'counsel-file-jump
   :desc "Find file in project"     :n "/"  #'projectile-find-file
   :desc "Find in file (swiper)"    :n "?"  #'swiper
   :desc "Imenu"                    :n ";"  #'imenu
   :desc "Imenu across buffers"     :n ":"  #'imenu-anywhere
   :desc "Find other file"          :n "a"  #'projectile-find-other-file
   :desc "Jump to bookmark"         :n "b"  #'bookmark-jump
   :desc "Delete bookmark"          :n "B"  #'bookmark-delete
   :desc "List errors"              :n "e"  #'flycheck-list-errors
   :desc "View Emacs Log"           :n "m"  #'view-echo-area-messages
   :desc "Recent files"             :n "r"  #'recentf
   :desc "Recent project files"     :n "R"  #'projectile-recentf
   :desc "Open file explorer"       :n "n"  #'+evil/neotree
   :desc "Insert from kill ring"    :n "y"  #'counsel-yank-pop
   :desc "Switch project"           :n "p"  #'projectile-switch-project
   :desc "Find snippet for mode"    :n "s"  #'yas-visit-snippet-file
   :desc "Find snippet"             :n "S"  #'+hlissner/find-in-snippets
   :desc "Execute in Emacs mode"    :n "\\" #'evil-execute-in-emacs-state
   :desc "Switch to Emacs mode"     :n "|"  #'evil-emacs-state
   ;; Since I've remapped C-h...
   :desc "Help"                     :n "h"  #'help-command

   (:desc "Session/Workspace"
     :prefix "w"
     :desc "Load last session"      :n "l" (λ! (+workspace/load-session))
     :desc "Load session"           :n "L" #'+workspace/load-session
     :desc "Save session"           :n "s" #'+workspace/save-session
     :desc "Delete session"         :n "X" #'+workspace/kill-session
     :desc "Load workspace"         :n "L" #'+workspace/load
     :desc "Save workspace"         :n "L" #'+workspace/save
     :desc "Delete workspace"       :n "x" #'+workspace/delete
     :desc "Switch workspace"       :n "." #'+workspace/switch-to
     :desc "Kill all buffers"       :n "x" #'doom/kill-all-buffers)

   (:desc "Quit"
     :prefix "q"
     :desc "Quit"                   :n "q" #'evil-save-and-quit
     :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

   (:desc "Toggle"
     :prefix "t"
     :desc "Spell check"            :n "s" #'flyspell-mode
     :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
     :desc "Fullscreen"             :n "f" #'doom/toggle-fullscreen
     :desc "Indent guides"          :n "i" #'highlight-indentation-mode
     :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"         :n "h" #'+present/impatient-mode
     :desc "Big mode"               :n "b" #'+present/big-mode)

   (:desc "Tmux/Terminal"
     :prefix "T"
     :desc "cd to here"             :n "." #'+tmux/cd-to-here
     :desc "cd to project"          :n "/" #'+tmux/cd-to-project
     ;; TODO :desc "ssh into..."    :n "/" '+tmux/ssh
     :desc "send selection"         :v "r" #'+tmux/send-region)

   (:desc "SSH/FTP"
     :prefix "u"
     :desc "Upload local"           :n "u" #'+upload/local
     :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
     :desc "Download remote"        :n "d" #'+upload/remote-download
     :desc "Diff local & remote"    :n "D" #'+upload/diff
     :desc "Browse remote files"    :n "." #'+upload/browse
     :desc "Detect remote changes"  :n "." #'+upload/check-remote)

   (:desc "Open with"
     :prefix "o"
     :desc "Default browser"        :n "b" #'browse-url-of-file
     (:when IS-MAC
       :desc "Reveal in Finder"          :n "o" #'+macos/reveal
       :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project
       :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
       :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
       :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
       :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

   (:desc "Code tools"
     :prefix "c"
     :desc "Build"                  :n  "b"  #'+eval/build
     :desc "Open/Send to REPL"      :nv "r"  #'+eval/repl
     :desc "Open debugger"          :n  "R"  #'+debug/open)

   (:desc "Personal"
     :prefix "SPC"
     :desc "Browse emacs.d"         :n "."   #'+hlissner/browse-emacsd
     :desc "Find file in emacs.d"   :n "/"   #'+hlissner/find-in-emacsd
     :desc "Browse dotfiles"        :n ">"   #'+hlissner/browse-dotfiles
     :desc "Find file in dotfiles"  :n "?"   #'+hlissner/find-in-dotfiles
     :desc "Reload theme"           :n "R"   #'+doom/reset-theme
     ;; Org notes
     :desc "Browse notes"           :n "n"   #'+hlissner/browse-notes
     :desc "Find file in notes"     :n "N"   #'+hlissner/find-in-notes
     :desc "Browse project notes"   :n "p"   #'+org/browse-notes-for-project
     :desc "Browse mode notes"      :n "m"   #'+org/browse-notes-for-major-mode
     :desc "Org Capture"            :n "SPC" #'+org/capture
     ;; applications
     :desc "APP: elfeed"            :n "r"   #'=rss
     :desc "APP: email"             :n "e"   #'=email
     :desc "APP: twitter"           :n "t"   #'=twitter
     ))

 (:localleader
  (:desc "Refactor..." :prefix "r")

  (:desc "Find..."     :prefix "f"
    :desc "Get type..." :prefix "t")

  (:desc "Unit tests"  :prefix "t"))

 ;;; Evil-esque bindings
 ;; Yank to EOL
 :n  "Y"  "y$"
 ;; Folding
 :n  "zr" #'+evil:open-folds-recursively
 :n  "zm" #'+evil:close-folds-recursively
 :n  "zx" #'doom/kill-this-buffer
 ;; Buffers
 :n  "ZX" #'bury-buffer
 :n  "]b" #'doom/next-buffer
 :n  "[b" #'doom/previous-buffer
 ;; Diffs
 :m  "]d" #'git-gutter:next-hunk
 :m  "[d" #'git-gutter:previous-hunk
 :m  "]e" #'next-error
 :m  "[e" #'previous-error
 ;; Switch tabs
 :n  "]w" #'+workspace/switch-right
 :n  "[w" #'+workspace/switch-left
 :m  "gt" #'+workspace/switch-right
 :m  "gT" #'+workspace/switch-left
 ;; Increment/decrement number under cursor
 :n  "g=" #'evil-numbers/inc-at-pt
 :n  "g-" #'evil-numbers/dec-at-pt
 ;; Todos
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous
 ;; Navigation
 :nv "K"  #'smart-up
 :m  "gd" #'+jump/definition
 :m  "gD" #'+jump/references
 :n  "gf" #'find-file-at-point
 :n  "gp" #'+evil/reselect-paste
 :n  "gc" #'evil-commentary
 :n  "gx" #'evil-exchange
 :n  "gr" #'+eval:region
 :n  "gR" #'+eval/buffer
 :v  "gR" #'+eval:replace-region
 :m  "g]" #'smart-forward
 :m  "g[" #'smart-backward
 :v  "@"  #'+evil:macro-on-all-lines
 :n  "g@" #'+evil:macro-on-all-lines
 ;; Repeat in visual mode (buggy)
 :v  "."  #'evil-repeat
 :v  "<"  #'+evil/visual-dedent     ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent     ; vnoremap > >gv
 ;; undo/redo for regions (buggy)
 :nv "u"   #'undo-tree-undo
 :nv "C-r" #'undo-tree-redo
 ;; paste from recent yank register (which isn't overwritten)
 :v  "C-p" "\"0p"

 (:map evil-window-map ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-zoom
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window)

 ;;; Plugins
 ;; company-mode (+ vim-like omnicompletion)
 :i "C-SPC"  #'+company/complete
 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "C-]"   #'company-tags
   :i "s"     #'company-ispell
   :i "C-s"   #'company-yasnippet
   :i "C-o"   #'company-capf
   :i "C-n"   #'company-dabbrev-code
   :i "C-p"   (λ! (let ((company-selection-wrap-around t))
                    (call-interactively 'company-dabbrev-code)
                    (company-select-previous-or-abort))))
 ;; evil-visual-star
 :v  "*"   #'evil-visualstar/begin-search-forward
 :v  "#"   #'evil-visualstar/begin-search-backward
 ;; evil-surround
 :v  "S"   #'evil-surround-region
 :o  "s"   #'evil-surround-edit
 :o  "S"   #'evil-Surround-edit
 ;; expand-region
 :v  "v"   #'er/expand-region
 :v  "V"   #'er/contract-region
 ;; rotate-text
 :n  "!"   #'rotate-text
 ;; evil-matchit
 :m  "%"   #'evilmi-jump-items
 ;; hide-show/evil-matchit
 :nv "<tab>" #'+evil/matchit-or-toggle-fold

 ;; help-mode
 (:map help-mode-map
   :n "]]"  #'help-go-forward
   :n "[["  #'help-go-back
   :n "o"   #'ace-link-help))


;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:unless window-system "TAB" [tab]) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; Restore 'dumb' indentation to the tab key. This rustles a lot of peoples'
      ;; jimmies, apparently, but it's how I like it.
      :i "<tab>"     #'doom/dumb-indent
      :i "<backtab>" #'doom/dumb-dedent
      :i "<C-tab>"   #'indent-for-tab-command
      :i "<A-tab>"   (λ! (insert "\t"))
      ;; 'smart' indentation for lisp modes
      (:after lisp-mode
        (:map lisp-mode-map       :i [remap doom/dumb-indent] #'indent-for-tab-command))
      (:after elisp-mode
        (:map emacs-lisp-mode-map :i [remap doom/dumb-indent] #'indent-for-tab-command))

      ;; textmate-esque newline insertion
      :i [M-return]     #'evil-open-below
      :i [S-M-return]   #'evil-open-above
      ;; Textmate-esque newlines
      :i [backspace]    #'delete-backward-char
      :i [M-backspace]  #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word
      ;; escape from insert mode (more responsive than using key-chord-define)
      :irv "C-g" #'evil-normal-state

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i "SPC"                          #'doom/inflate-space-maybe
      :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      :i [remap newline]                #'doom/newline-and-indent

      ;; Make ESC quit all the things
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register)

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:map (evil-ex-completion-map evil-ex-search-keymap read-expression-map)
        "C-a" #'move-beginning-of-line
        "C-w" #'doom-minibuffer-kill-word
        "C-u" #'doom-minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom-minibuffer-undo)

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all))

      (:map help-map
        "l" #'find-library
        "h" #'describe-face  ; overwrite `view-hello-file'
        "g" nil))

