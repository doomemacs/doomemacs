;;; my-bindings.el

;; Minimalistic key mapping! Why go so far for this?
;; ...
;; Uh. Good question.

(eval-when-compile (require 'core-defuns))

;; See `narf-leader-prefix' & `narf-localleader-prefix' in ../core/core.el

(map!
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Global keymaps                     ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 "M-x"  'helm-M-x
 "M-;"  'eval-expression
 "M-/"  'evil-commentary-line
 "A-x"  'helm-M-x
 "A-;"  'eval-expression
 "A-/"  'evil-commentary-line

 "M-0"  (λ! (text-scale-set 0))
 "M-="  'text-scale-increase
 "M--"  'text-scale-decrease

 ;; debug
 "<f9>" 'what-face

 "M-b"  'narf:build
 "M-t"  'narf:tab-create
 "M-T"  'narf/tab-display
 "A-`"  'os-switch-to-term
 "C-`"  'narf/popup-messages
 "C-~"  'narf:repl
 "M-`"  'narf/popup-toggle

 "M-w"  'narf/close-window-or-tab
 "M-W"  'delete-frame
 "M-n"  'narf/new-buffer
 "M-N"  'narf/new-frame

 ;; Simpler window navigation
 "C-j"  'evil-window-down
 "C-k"  'evil-window-up
 "C-h"  'evil-window-left
 "C-l"  'evil-window-right

 "C-<escape>" 'evil-emacs-state
 :e "C-<escape>" 'evil-normal-state

 :m "M-1"  (λ! (narf:switch-to-tab 0))
 :m "M-2"  (λ! (narf:switch-to-tab 1))
 :m "M-3"  (λ! (narf:switch-to-tab 2))
 :m "M-4"  (λ! (narf:switch-to-tab 3))
 :m "M-5"  (λ! (narf:switch-to-tab 4))
 :m "M-6"  (λ! (narf:switch-to-tab 5))
 :m "M-7"  (λ! (narf:switch-to-tab 6))
 :m "M-8"  (λ! (narf:switch-to-tab 7))
 :m "M-9"  (λ! (narf:switch-to-tab 8))

 (:when IS-MAC
   "<A-left>"       'backward-word
   "<A-right>"      'forward-word
   "<M-backspace>"  'narf/backward-kill-to-bol-and-indent
   "A-SPC"          'just-one-space
   "M-a"            'mark-whole-buffer
   "M-c"            'evil-yank
   "M-o"            'helm-find-files
   "M-q"            'evil-quit-all
   "M-s"            'save-buffer
   "M-v"            'clipboard-yank
   "M-z"            'undo
   "M-Z"            'redo
   "C-M-f"          'narf:toggle-fullscreen

   :m "M-j"  'narf/multi-next-line
   :m "M-k"  'narf/multi-previous-line

   :n "M-r"  'narf:eval-buffer
   :v "M-r"  'narf:eval-region

   :ni "<M-f1>" 'narf/dash-at-pt

   ;; Textmate-esque indent shift left/right
   :i "M-]"   'narf/smart-indent
   :i "M-["   'narf/dumb-dedent

   ;; Restore osx text objects
   :i "<A-backspace>" 'evil-delete-backward-word
   :i "<A-delete>"    (λ! (evil-forward-word) (evil-delete-backward-word)))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Local keymaps                      ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 :m ";" 'evil-ex
 (:leader
   :nv ","  'narf/helm-buffers-dwim
   :nv "<"  'helm-buffers-list
   :nv "."  'helm-find-files
   :nv ">"  'helm-projectile-find-file-in-known-projects
   :nv "/"  'helm-projectile-find-file
   :nv ";"  'helm-semantic-or-imenu
   :nv ":"  'helm-imenu-in-all-buffers
   :nv "]"  'helm-etags-select
   :nv "a"  'helm-projectile-find-other-file
   :nv "m"  'helm-recentf
   :nv "M"  'helm-projectile-recentf  ; recent PROJECT files
   :nv "P"  'helm-projectile-switch-project
   :v  "="  'align-regexp

   :n  "h"  'help-command
   :nv "p"  'helm-show-kill-ring
   :n  "R"  'narf/reset-theme
   :n  "e"  'narf/flycheck-errors
   :n  "s"  'yas-visit-snippet-file
   :n  "S"  'narf/yas-find-file
   :n  "D"  'vc-annotate
   (:prefix "d"
     :n "." 'narf/vcs-show-hunk
     :n "/" 'vc-diff
     :n "s" 'narf/vcs-stage-hunk
     :n "r" 'narf/vcs-revert-hunk)

   :n  "b"  'helm-bookmarks
   :nv "l"  'narf/nlinum-toggle

   :nv "qq" 'evil-save-and-quit
   :nv "QQ" 'narf/kill-all-buffers-do-not-remember

   ;; Quick access to my dotfiles and emacs.d
   :nv "E"  'narf/helm-find-in-emacsd
   :nv "\\" 'narf/helm-find-in-dotfiles

   ;; Tmux
   (:prefix "t"
     :n  "." 'narf/tmux-cd-to-here
     :n  "/" 'narf/tmux-cd-to-project
     :v  "r" 'narf:tmux)

   ;; Open with O/S
   :n "O" 'os-reveal
   (:prefix "o"
     :n  "o" 'os-open-in-default-program
     :n  "p" 'os-reveal-project
     :n  "b" 'os-open-in-browser
     :n  "u" 'os-upload
     :n  "U" 'os-upload-folder
     :n  "l" 'os-send-to-launchbar
     :n  "L" 'os-send-project-to-launchbar
     :n  "t" 'os-switch-to-term
     :n  "T" 'os-switch-to-term-and-cd)

   ;; Org notes
   :n "X" 'narf/org-start
   (:prefix "x"
     :n "." 'narf/org-find-file
     :n "/" 'narf/org-find-file-in-notes
     :n "e" 'narf/org-find-exported-file))

 (:localleader
   :n "\\" 'narf/neotree
   :n "]" 'imenu-list-minor-mode
   :n "b" 'narf:build
   :n "R" 'narf:repl
   :v "R" 'narf:repl-eval
   :v "r" 'narf:eval-region
   (:prefix "r"
     :n  "e" 'emr-show-refactor-menu
     :n  "r" 'narf:eval-buffer))

 :nv "K"  'smart-up

 ;; Don't move cursor on indent
 :n  "="  (λ! (save-excursion (call-interactively 'evil-indent)))
 :v  "="  'evil-indent

 :n  "zr" 'narf/evil-open-folds
 :n  "zm" 'narf/evil-close-folds
 :n  "zx" 'narf/kill-real-buffer
 :n  "ZX" 'bury-buffer

 ;; These are intentionally reversed
 :n  "]b" 'narf/next-real-buffer
 :n  "[b" 'narf/previous-real-buffer

 :m  "]d" 'narf/vcs-next-hunk
 :m  "[d" 'narf/vcs-prev-hunk
 :m  "]e" 'narf/flycheck-next-error
 :m  "[e" 'narf/flycheck-previous-error
 ;; Switch tabs
 :n  "]w" 'narf:switch-to-tab-right
 :n  "[w" 'narf:switch-to-tab-left
 :m  "gt" 'narf:switch-to-tab-right
 :m  "gT" 'narf:switch-to-tab-left

 ;; Increment/decrement number under cursor
 :n  "g=" 'evil-numbers/inc-at-pt
 :n  "g-" 'evil-numbers/dec-at-pt

 ;; NOTE: Helm is too bulky for ffap (which I use for quick file navigation)
 :n  "gf" (λ! (helm-mode -1)
              (call-interactively 'find-file-at-point)
              (helm-mode 1))

 :n  "gp" 'narf/reselect-paste
 :n  "gc" 'evil-commentary
 :n  "gx" 'evil-exchange
 :n  "gr" 'narf:eval-region
 :n  "gR" 'narf:eval-buffer
 :v  "gR" 'narf:eval-region-and-replace
 :m  "g]" 'smart-right
 :m  "g[" 'smart-left
 :v  "@"  'narf/evil-macro-on-all-lines
 :n  "g@" 'narf/evil-macro-on-all-lines

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

 :v  "*"   'evil-visualstar/begin-search-forward
 :v  "#"   'evil-visualstar/begin-search-backward

 :n  "Y"   "y$"

 ;; paste from recent yank register; which isn't overwritten by deletes or
 ;; other operations.
 :v  "C-p" "\"0p"

 :v  "S"   'evil-surround-region
 :v  "v"   'er/expand-region
 :v  "V"   'er/contract-region

 ;; aliases for %
 :m  "%"   'evilmi-jump-items
 :m [tab] (λ! (if (ignore-errors (hs-already-hidden-p))
                  (hs-toggle-hiding)
                (call-interactively 'evilmi-jump-items)))

 ;; Textmate-esque newlines
 :i  "<backspace>"   'backward-delete-char-untabify
 :i  "<M-backspace>" 'narf/backward-kill-to-bol-and-indent
 :i  "<C-return>"    'evil-ret-and-indent

 ;; Emacsien motions for insert mode
 :i "C-b" 'backward-word
 :i "C-f" 'forward-word

 ;; escape from insert mode (more responsive than using key-chord-define)
 :irv "C-g"  'evil-normal-state

 :o  "s"     'evil-surround-edit
 :o  "S"     'evil-Surround-edit

 :n  "!"     'rotate-text

 (:map evil-window-map ; prefix "C-w"
   "u"       'narf/undo-window-change

   ;; Jump to new splits
   "s"       'narf/evil-window-split
   "v"       'narf/evil-window-vsplit

   ;; Move window in one step
   "H"       'narf/evil-window-move-left
   "J"       'narf/evil-window-move-down
   "K"       'narf/evil-window-move-up
   "L"       'narf/evil-window-move-right

   "C-u"     'narf/undo-window-change
   "C-r"     'narf/redo-window-change
   "C-h"     'evil-window-left
   "C-j"     'evil-window-down
   "C-k"     'evil-window-up
   "C-l"     'evil-window-right

   "C-w"     'ace-window
   "C-S-w"   (λ! (ace-window 4))    ; swap windows
   "C-C"     (λ! (ace-window 16)))  ; delete windows

 ;; Vim omni-complete emulation
 :i "C-SPC" 'narf/company-complete
 (:prefix "C-x"
   :i "C-l"   'narf/company-whole-lines
   :i "C-k"   'narf/company-dict-or-keywords
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
     [tab]        'narf/company-complete-common-or-complete-full
     "<backtab>"  'company-select-previous
     [escape]     (λ! (company-abort) (evil-normal-state 1))
     "<C-return>" 'helm-company)
   (:map company-search-map
     "C-n"        'company-search-repeat-forward
     "C-p"        'company-search-repeat-backward
     [escape]     'company-search-abort))

 (:after help-mode
   (:map help-map
     "e" 'narf/popup-messages
     ;; Remove slow/annoying help subsections
     "h" nil
     "g" nil)
   (:map help-mode-map
     :n "]]" 'help-go-forward
     :n "[[" 'help-go-back
     :n "<escape>" 'narf/popup-close)))

;; Line-wise mouse selection on margin
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'narf/mouse-drag-line)
(global-set-key (kbd "<left-margin> <mouse-1>")      'narf/mouse-select-line)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'narf/mouse-select-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap fixes                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section is dedicated to bindings that "fix" certain keys so that they behave more
;; like vim (or how I like it).

;; Restores "dumb" indentation to the tab key. This rustles a lot of peoples' jimmies,
;; apparently, but it's how I like it.
(map! :i "<tab>"     'narf/dumb-indent
      :i "<backtab>" 'narf/dumb-dedent
      :i "<C-tab>"   'indent-for-tab-command

      ;; No dumb-tab for lisp
      (:map lisp-mode-map        :i [remap narf/dumb-indent] 'indent-for-tab-command)
      (:map emacs-lisp-mode-map  :i [remap narf/dumb-indent] 'indent-for-tab-command)

      ;; Highjacks space/backspace to:
      ;;   a) delete spaces on either side of the cursor, if present ( | ) -> (|)
      ;;   b) allow backspace to delete space-indented blocks intelligently
      ;;   c) and not do any of this magic when inside a string
      :i "SPC"                                  'narf/inflate-space-maybe
      :i [remap backward-delete-char-untabify]  'narf/deflate-space-maybe
      :i [remap newline]                        'narf/newline-and-indent

      ;; Smarter move-to-beginning-of-line
      :i [remap move-beginning-of-line]         'narf/move-to-bol

      ;; Restore bash-esque keymaps in insert mode; C-w and C-a already exist
      :i "C-e" 'narf/move-to-eol
      :i "C-u" 'narf/backward-kill-to-bol-and-indent

      ;; Fixes delete
      :i "<kp-delete>" 'delete-char

      ;; Fix osx keymappings and then some
      :i "<M-left>"   'narf/move-to-bol
      :i "<M-right>"  'narf/move-to-eol
      :i "<M-up>"     'beginning-of-buffer
      :i "<M-down>"   'end-of-buffer
      :i "<C-up>"     'smart-up
      :i "<C-down>"   'smart-down

      ;; Fix emacs motion keys
      :i "A-b"      'evil-backward-word-begin
      :i "A-w"      'evil-forward-word-begin
      :i "A-e"      'evil-forward-word-end

      ;; Textmate-esque insert-line before/after
      :i "<M-return>"    'evil-open-below
      :i "<S-M-return>"  'evil-open-above
      ;; insert lines in-place)
      :n "<M-return>"    (λ! (save-excursion (evil-insert-newline-below)))
      :n "<S-M-return>"  (λ! (save-excursion (evil-insert-newline-above)))

      ;; Make ESC quit all the things
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map)
        [escape] 'narf-minibuffer-quit
        "C-r" 'evil-paste-from-register)

      (:map read-expression-map "C-w" 'backward-kill-word)

      (:map view-mode-map "<escape>" 'View-quit-all)

      (:map evil-ex-completion-map "C-a" 'move-beginning-of-line))

;; Common unicode characters
(map! :i "A-o" (λ! (insert "ø"))
      :i "A-O" (λ! (insert "Ø"))

      :i "A--" (λ! (insert "–"))
      :i "A-_" (λ! (insert "—")))

(provide 'my-bindings)
;;; my-bindings.el ends here
