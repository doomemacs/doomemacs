;;; config/default/+bindings.el -*- lexical-binding: t; -*-

;; This file defines a Spacemacs-esque keybinding scheme

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "C-j" "C-k" "gd" "gf" "K" "[" "]" "gz"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))


;;
;;; Global keybindings

(map! (:map override
        ;; A little sandbox to run code in
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      ;; Smart tab, these will only work in GUI Emacs
      :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
                 (and (featurep! :editor snippets)
                      (bound-and-true-p yas-minor-mode)
                      (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                 'yas-expand
                 (and (featurep! :completion company +tng)
                      (+company-has-completion-p))
                 '+company/complete)
      :n [tab] (general-predicate-dispatch nil
                 (and (featurep! :editor fold)
                      (save-excursion (end-of-line) (invisible-p (point))))
                 '+fold/toggle
                 (fboundp 'evilmi-jump-items)
                 'evilmi-jump-items)
      :v [tab] (general-predicate-dispatch nil
                 (and (bound-and-true-p yas-minor-mode)
                      (or (eq evil-visual-selection 'line)
                          (and (fboundp 'evilmi-jump-items)
                               (save-excursion
                                 (/= (point)
                                     (progn (evilmi-jump-items nil)
                                            (point)))))))
                 'yas-insert-snippet
                 (fboundp 'evilmi-jump-items)
                 'evilmi-jump-items)

      ;; Smarter newlines
      :i [remap newline] #'newline-and-indent  ; auto-indent on newline
      :i "C-j"           #'+default/newline    ; default behavior

      (:after vc-annotate
        :map vc-annotate-mode-map
        [remap quit-window] #'kill-current-buffer)

      (:map (help-mode-map helpful-mode-map)
        :n "o" 'ace-link-help)

      ;; misc
      :n "C-S-f"  #'toggle-frame-fullscreen

      ;; Global evil keybinds
      :m  "]a"    #'evil-forward-arg
      :m  "[a"    #'evil-backward-arg
      :m  "]o"    #'outline-next-visible-heading
      :m  "[o"    #'outline-previous-visible-heading
      :n  "]b"    #'next-buffer
      :n  "[b"    #'previous-buffer
      :n  "zx"    #'kill-current-buffer
      :n  "ZX"    #'bury-buffer
      :n  "gp"    #'+evil/reselect-paste
      :n  "g="    #'evil-numbers/inc-at-pt
      :n  "g-"    #'evil-numbers/dec-at-pt
      :v  "g="    #'evil-numbers/inc-at-pt-incremental
      :v  "g-"    #'evil-numbers/dec-at-pt-incremental
      :v  "g+"    #'evil-numbers/inc-at-pt
      :nv "z="    #'flyspell-correct-word-generic
      :nv "g@"    #'+evil:apply-macro
      :nv "gc"    #'evil-commentary
      :nv "gx"    #'evil-exchange
      :v  "gp"    #'+evil/paste-preserve-register
      :v  "@"     #'+evil:apply-macro
      ;; repeat in visual mode (FIXME buggy)
      :v  "."     #'+evil:apply-macro
      ;; don't leave visual mode after shifting
      :v  "<"     #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"     #'+evil/visual-indent  ; vnoremap > >gv

      ;; window management (prefix "C-w")
      (:map evil-window-map
        ;; Navigation
        "C-h"     #'evil-window-left
        "C-j"     #'evil-window-down
        "C-k"     #'evil-window-up
        "C-l"     #'evil-window-right
        "C-w"     #'other-window
        ;; Swapping windows
        "H"       #'+evil/window-move-left
        "J"       #'+evil/window-move-down
        "K"       #'+evil/window-move-up
        "L"       #'+evil/window-move-right
        "C-S-w"   #'ace-swap-window
        ;; Window undo/redo
        (:prefix "m"
          "m"       #'doom/window-maximize-buffer
          "v"       #'doom/window-maximize-vertically
          "s"       #'doom/window-maximize-horizontally)
        "u"       #'winner-undo
        "C-u"     #'winner-undo
        "C-r"     #'winner-redo
        "o"       #'doom/window-enlargen
        ;; Delete window
        "c"       #'+workspace/close-window-or-workspace
        "C-C"     #'ace-delete-window)

      ;; Plugins
      ;; evil-easymotion
      :m  "gs"    #'+evil/easymotion  ; lazy-load `evil-easymotion'
      (:after evil-easymotion
        :map evilem-map
        "a" (evilem-create #'evil-forward-arg)
        "A" (evilem-create #'evil-backward-arg)
        "s" (evilem-create #'evil-snipe-repeat
                           :name 'evil-easymotion-snipe-forward
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight)))
        "S" (evilem-create #'evil-snipe-repeat
                           :name 'evil-easymotion-snipe-backward
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-S))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight)))
        "SPC" #'avy-goto-char-timer
        "/" (evilem-create #'evil-ex-search-next
                           :pre-hook (save-excursion (call-interactively #'evil-ex-search-forward))
                           :bind ((evil-search-wrap)))
        "?" (evilem-create #'evil-ex-search-previous
                           :pre-hook (save-excursion (call-interactively #'evil-ex-search-backward))
                           :bind ((evil-search-wrap))))

      ;; text object plugins
      :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

      ;; evil-snipe
      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))

      ;; evil-surround
      :v "S" #'evil-surround-region
      :o "s" #'evil-surround-edit
      :o "S" #'evil-Surround-edit)


;;
;;; Module keybinds

;;; :completion
(map! (:when (featurep! :completion company)
        :i "C-@"      #'+company/complete
        :i "C-SPC"    #'+company/complete
        (:prefix "C-x"
          :i "C-l"    #'+company/whole-lines
          :i "C-k"    #'+company/dict-or-keywords
          :i "C-f"    #'company-files
          :i "C-]"    #'company-etags
          :i "s"      #'company-ispell
          :i "C-s"    #'company-yasnippet
          :i "C-o"    #'company-capf
          :i "C-n"    #'+company/dabbrev
          :i "C-p"    #'+company/dabbrev-code-previous)
        (:after company
          (:map company-active-map
            "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
            "C-n"     #'company-select-next
            "C-p"     #'company-select-previous
            "C-j"     #'company-select-next
            "C-k"     #'company-select-previous
            "C-h"     #'company-show-doc-buffer
            "C-u"     #'company-previous-page
            "C-d"     #'company-next-page
            "C-s"     #'company-filter-candidates
            "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                            ((featurep! :completion ivy)  #'counsel-company))
            "C-SPC"   #'company-complete-common
            "TAB"     #'company-complete-common-or-cycle
            [tab]     #'company-complete-common-or-cycle
            [backtab] #'company-select-previous)
          (:map company-search-map  ; applies to `company-filter-map' too
            "C-n"     #'company-select-next-or-abort
            "C-p"     #'company-select-previous-or-abort
            "C-j"     #'company-select-next-or-abort
            "C-k"     #'company-select-previous-or-abort
            "C-s"     (λ! (company-search-abort) (company-filter-candidates))
            "ESC"     #'company-search-abort)
          ;; TAB auto-completion in term buffers
          (:map comint-mode-map
            "TAB" #'company-complete
            [tab] #'company-complete)))

      (:when (featurep! :completion ivy)
        (:map (help-mode-map helpful-mode-map)
          :n "Q" #'ivy-resume)
        (:after ivy
          :map ivy-minibuffer-map
          "C-SPC" #'ivy-call-and-recenter  ; preview file
          "C-l"   #'ivy-alt-done
          "C-v"   #'yank)
        (:after counsel
          :map counsel-ag-map
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "C-l"      #'ivy-done
          [C-return] (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

      (:when (featurep! :completion helm)
        (:after helm
          (:map helm-map
            [left]     #'left-char
            [right]    #'right-char
            "C-S-n"    #'helm-next-source
            "C-S-p"    #'helm-previous-source
            "C-j"      #'helm-next-line
            "C-k"      #'helm-previous-line
            "C-S-j"    #'helm-next-source
            "C-S-k"    #'helm-previous-source
            "C-f"      #'helm-next-page
            "C-S-f"    #'helm-previous-page
            "C-u"      #'helm-delete-minibuffer-contents
            "C-w"      #'backward-kill-word
            "C-r"      #'evil-paste-from-register ; Evil registers in helm! Glorious!
            "C-s"      #'helm-minibuffer-history
            "C-b"      #'backward-word
            ;; Swap TAB and C-z
            "TAB"      #'helm-execute-persistent-action
            [tab]      #'helm-execute-persistent-action
            "C-z"      #'helm-select-action)
          (:after helm-ag
            :map helm-ag-map
            "C--"      #'+helm-do-ag-decrease-context
            "C-="      #'+helm-do-ag-increase-context
            [left]     nil
            [right]    nil)
          (:after helm-files
            :map (helm-find-files-map helm-read-file-map)
            [C-return] #'helm-ff-run-switch-other-window
            "C-w"      #'helm-find-files-up-one-level)
          (:after helm-locate
            :map helm-generic-files-map
            [C-return] #'helm-ff-run-switch-other-window)
          (:after helm-buffers
            :map helm-buffer-map
            [C-return] #'helm-buffer-switch-other-window)
          (:after helm-occur
            :map helm-occur-map
            [C-return] #'helm-occur-run-goto-line-ow)
          (:after helm-grep
            :map helm-grep-map
            [C-return] #'helm-grep-run-other-window-action))))

;;; :ui
(map! (:when (featurep! :ui hl-todo)
        :m "]t" #'hl-todo-next
        :m "[t" #'hl-todo-previous)

      (:when (featurep! :ui neotree)
        :after neotree
        :map neotree-mode-map
        :n "g"      nil
        :n "TAB"    #'neotree-quick-look
        :n "RET"    #'neotree-enter
        :n [tab]    #'neotree-quick-look
        :n [return] #'neotree-enter
        :n "DEL"    #'evil-window-prev
        :n "c"      #'neotree-create-node
        :n "r"      #'neotree-rename-node
        :n "d"      #'neotree-delete-node
        :n "j"      #'neotree-next-line
        :n "k"      #'neotree-previous-line
        :n "n"      #'neotree-next-line
        :n "p"      #'neotree-previous-line
        :n "h"      #'+neotree/collapse-or-up
        :n "l"      #'+neotree/expand-or-open
        :n "J"      #'neotree-select-next-sibling-node
        :n "K"      #'neotree-select-previous-sibling-node
        :n "H"      #'neotree-select-up-node
        :n "L"      #'neotree-select-down-node
        :n "G"      #'evil-goto-line
        :n "gg"     #'evil-goto-first-line
        :n "v"      #'neotree-enter-vertical-split
        :n "s"      #'neotree-enter-horizontal-split
        :n "q"      #'neotree-hide
        :n "R"      #'neotree-refresh)

      (:when (featurep! :ui popup)
        :n "C-`"   #'+popup/toggle
        :n "C-~"   #'+popup/raise
        :g "C-x p" #'+popup/other)

      (:when (featurep! :ui vc-gutter)
        :m "]d"    #'git-gutter:next-hunk
        :m "[d"    #'git-gutter:previous-hunk)

      (:when (featurep! :ui workspaces)
        :n "gt"    #'+workspace/switch-right
        :n "gT"    #'+workspace/switch-left
        :n "]w"    #'+workspace/switch-right
        :n "[w"    #'+workspace/switch-left
        :g "M-1"   (λ! (+workspace/switch-to 0))
        :g "M-2"   (λ! (+workspace/switch-to 1))
        :g "M-3"   (λ! (+workspace/switch-to 2))
        :g "M-4"   (λ! (+workspace/switch-to 3))
        :g "M-5"   (λ! (+workspace/switch-to 4))
        :g "M-6"   (λ! (+workspace/switch-to 5))
        :g "M-7"   (λ! (+workspace/switch-to 6))
        :g "M-8"   (λ! (+workspace/switch-to 7))
        :g "M-9"   (λ! (+workspace/switch-to 8))
        :g "M-0"   #'+workspace/switch-to-last
        :g "M-t"   #'+workspace/new
        :g "M-T"   #'+workspace/display))

;;; :editor
(map! (:when (featurep! :editor fold)
        :nv "C-SPC" #'+fold/toggle)

      (:when (featurep! :editor format)
        :n "gQ"    #'+format:region)

      (:when (featurep! :editor multiple-cursors)
        ;; evil-mc
        (:prefix "gz"
          :nv "d" #'evil-mc-make-and-goto-next-match
          :nv "D" #'evil-mc-make-and-goto-prev-match
          :nv "j" #'evil-mc-make-cursor-move-next-line
          :nv "k" #'evil-mc-make-cursor-move-prev-line
          :nv "m" #'evil-mc-make-all-cursors
          :nv "n" #'evil-mc-make-and-goto-next-cursor
          :nv "N" #'evil-mc-make-and-goto-last-cursor
          :nv "p" #'evil-mc-make-and-goto-prev-cursor
          :nv "P" #'evil-mc-make-and-goto-first-cursor
          :nv "q" #'evil-mc-undo-all-cursors
          :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
          :nv "u" #'evil-mc-undo-last-added-cursor
          :nv "z" #'+multiple-cursors/evil-mc-make-cursor-here)
        (:after evil-mc
          :map evil-mc-key-map
          :nv "C-n" #'evil-mc-make-and-goto-next-cursor
          :nv "C-N" #'evil-mc-make-and-goto-last-cursor
          :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
          :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
        ;; evil-multiedit
        :v  "R"     #'evil-multiedit-match-all
        :n  "M-d"   #'evil-multiedit-match-symbol-and-next
        :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
        :v  "M-d"   #'evil-multiedit-match-and-next
        :v  "M-D"   #'evil-multiedit-match-and-prev
        :nv "C-M-d" #'evil-multiedit-restore
        (:after evil-multiedit
          (:map evil-multiedit-state-map
            "M-d"    #'evil-multiedit-match-and-next
            "M-D"    #'evil-multiedit-match-and-prev
            "RET"    #'evil-multiedit-toggle-or-restrict-region
            [return] #'evil-multiedit-toggle-or-restrict-region)
          (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
            "C-n" #'evil-multiedit-next
            "C-p" #'evil-multiedit-prev)))

      (:when (featurep! :editor rotate-text)
        :n "!" #'rotate-text)

      (:when (featurep! :editor snippets)
        ;; auto-yasnippet
        :i  [C-tab] #'aya-expand
        :nv [C-tab] #'aya-create
        ;; yasnippet
        (:after yasnippet
          (:map yas-keymap
            "C-e"         #'+snippets/goto-end-of-field
            "C-a"         #'+snippets/goto-start-of-field
            [M-right]     #'+snippets/goto-end-of-field
            [M-left]      #'+snippets/goto-start-of-field
            [M-backspace] #'+snippets/delete-to-start-of-field
            [backspace]   #'+snippets/delete-backward-char
            [delete]      #'+snippets/delete-forward-char-or-field))))

;;; :emacs
(map! (:when (featurep! :emacs vc)
        :after git-timemachine
        :map git-timemachine-mode-map
        :n "C-p" #'git-timemachine-show-previous-revision
        :n "C-n" #'git-timemachine-show-next-revision
        :n "[["  #'git-timemachine-show-previous-revision
        :n "]]"  #'git-timemachine-show-next-revision
        :n "q"   #'git-timemachine-quit
        :n "gb"  #'git-timemachine-blame))

;;; :tools
(map! (:when (featurep! :tools debugger)
        :after realgud
        :map realgud:shortkey-mode-map
        :n "j" #'evil-next-line
        :n "k" #'evil-previous-line
        :n "h" #'evil-backward-char
        :n "l" #'evil-forward-char
        :n "c" #'realgud:cmd-continue
        :m "n" #'realgud:cmd-next
        :m "b" #'realgud:cmd-break
        :m "B" #'realgud:cmd-clear)

      (:when (featurep! :tools eval)
        :g  "M-r" #'+eval/buffer
        :nv "gr"  #'+eval:region
        :n  "gR"  #'+eval/buffer
        :v  "gR"  #'+eval:replace-region)

      (:when (featurep! :tools flyspell)
        ;; Keybinds that have no Emacs+evil analogues (i.e. don't exist):
        ;;   zq - mark word at point as good word
        ;;   zw - mark word at point as bad
        ;;   zu{q,w} - undo last marking
        ;; Keybinds that evil define:
        ;;   z= - correct flyspell word at point
        ;;   ]s - jump to previous spelling error
        ;;   [s - jump to next spelling error
        (:map flyspell-mouse-map
          "RET"     #'flyspell-correct-word-generic
          [return]  #'flyspell-correct-word-generic
          [mouse-1] #'flyspell-correct-word-generic))

      (:when (featurep! :tools flycheck)
        :m "]e" #'next-error
        :m "[e" #'previous-error
        (:after flycheck
          :map flycheck-error-list-mode-map
          :n "C-n"    #'flycheck-error-list-next-error
          :n "C-p"    #'flycheck-error-list-previous-error
          :n "j"      #'flycheck-error-list-next-error
          :n "k"      #'flycheck-error-list-previous-error
          :n "RET"    #'flycheck-error-list-goto-error
          :n [return] #'flycheck-error-list-goto-error))

      (:when (featurep! :tools gist)
        :after gist
        :map gist-list-menu-mode-map
        :n "go"  #'gist-browse-current-url
        :n "gr"  #'gist-list-reload
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "e"   #'gist-edit-current-description
        :n "f"   #'gist-fork
        :n "q"   #'kill-current-buffer
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url)

      (:when (featurep! :tools lookup)
        :nv "K"  #'+lookup/documentation
        :nv "gd" #'+lookup/definition
        :nv "gD" #'+lookup/references
        :nv "gf" #'+lookup/file)

      (:when (featurep! :tools magit)
        (:after evil-magit
          ;; fix conflicts with private bindings
          :map (magit-status-mode-map magit-revision-mode-map)
          "C-j" nil
          "C-k" nil)
        (:map transient-map
          "q" #'transient-quit-one)))

;;; :lang
(map! (:when (featurep! :lang markdown)
        :after markdown-mode
        :map markdown-mode-map
        ;; fix conflicts with private bindings
        [backspace] nil))


;;
;;; <leader>

(map! :leader
      :desc "Eval expression"       ";"    #'eval-expression
      :desc "M-x"                   ":"    #'execute-extended-command
      :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "X"    #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "window"                "w"    evil-window-map
      :desc "help"                  "h"    help-map

      :desc "Toggle last popup"     "~"    #'+popup/toggle
      :desc "Find file"             "."    #'find-file

      :desc "Switch buffer"         ","    #'switch-to-buffer
      (:when (featurep! :ui workspaces)
        :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
        :desc "Switch buffer"           "<" #'switch-to-buffer)

      :desc "Resume last search"    "'"
      (cond ((featurep! :completion ivy)   #'ivy-resume)
            ((featurep! :completion helm)  #'helm-resume))

      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point

      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Blink cursor line"     "DEL"  #'+nav-flash/blink-cursor
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump

      ;;; <leader> / --- search
      (:prefix-map ("/" . "search")
        :desc "Search buffer"                 "b" #'swiper
        :desc "Search current directory"      "d" #'+default/search-from-cwd
        :desc "Jump to symbol"                "i" #'imenu
        :desc "Jump to link"                  "l" #'ace-link
        :desc "Look up online"                "o" #'+lookup/online-select
        :desc "Look up in local docsets"      "k" #'+lookup/in-docsets
        :desc "Look up in all docsets"        "K" #'+lookup/in-all-docsets
        :desc "Search project"                "p" #'+default/search-project)

      ;;; <leader> TAB --- workspace
      (:when (featurep! :ui workspaces)
        (:prefix-map ("TAB" . "workspace")
          :desc "Display tab bar"           "TAB" #'+workspace/display
          :desc "Switch workspace"          "."   #'+workspace/switch-to
          :desc "New workspace"             "n"   #'+workspace/new
          :desc "Load workspace from file"  "l"   #'+workspace/load
          :desc "Save workspace to file"    "s"   #'+workspace/save
          :desc "Delete session"            "x"   #'+workspace/kill-session
          :desc "Delete this workspace"     "d"   #'+workspace/delete
          :desc "Rename workspace"          "r"   #'+workspace/rename
          :desc "Restore last session"      "R"   #'+workspace/restore-last-session
          :desc "Next workspace"            "]"   #'+workspace/switch-right
          :desc "Previous workspace"        "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"   "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"   "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"   "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"   "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"   "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"   "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"   "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"   "8"   (λ! (+workspace/switch-to 7))
          :desc "Switch to 9th workspace"   "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace"  "0"   #'+workspace/switch-to-last))

      ;;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
        :desc "Toggle narrowing"            "-"   #'doom/clone-and-narrow-buffer
        :desc "Previous buffer"             "["   #'previous-buffer
        :desc "Next buffer"                 "]"   #'next-buffer
        (:when (featurep! :ui workspaces)
          :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           "B" #'switch-to-buffer)
        (:unless (featurep! :ui workspaces)
          :desc "Switch buffer"           "b" #'switch-to-buffer)
        :desc "Kill buffer"                 "k"   #'kill-current-buffer
        :desc "Next buffer"                 "n"   #'next-buffer
        :desc "New empty buffer"            "N"   #'evil-buffer-new
        :desc "Kill other buffers"          "o"   #'doom/kill-other-buffers
        :desc "Previous buffer"             "p"   #'previous-buffer
        :desc "Save buffer"                 "s"   #'save-buffer
        :desc "Sudo edit this file"         "S"   #'doom/sudo-this-file
        :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
        :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
        :desc "Bury buffer"                 "z"   #'bury-buffer)

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
        :desc "Compile"                     "c"   #'compile
        :desc "Jump to definition"          "d"   #'+lookup/definition
        :desc "Jump to references"          "D"   #'+lookup/references
        :desc "Evaluate buffer/region"      "e"   #'+eval/buffer-or-region
        :desc "Evaluate & replace region"   "E"   #'+eval:replace-region
        :desc "Format buffer/region"        "f"   #'+format/region-or-buffer
        :desc "Jump to documentation"       "k"   #'+lookup/documentation
        :desc "Open REPL"                   "r"   #'+eval/open-repl-other-window
        :desc "Delete trailing whitespace"  "w"   #'delete-trailing-whitespace
        :desc "Delete trailing newlines"    "W"   #'doom/delete-trailing-newlines
        :desc "List errors"                 "x"   #'flycheck-list-errors)

      ;;; <leader> f --- file
      (:prefix-map ("f" . "file")
        :desc "Find file"                   "."   #'find-file
        :desc "Find file from here"         "/"
        (if (featurep! :completion ivy)
            #'counsel-file-jump
          (λ! (doom-project-find-file default-directory)))
        :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
        :desc "Find directory"              "d"   #'dired
        :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
        :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
        :desc "Find file from here"         "f"   #'find-file
        :desc "Move/rename file"            "m"   #'doom/move-this-file
        :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
        :desc "Browse private config"       "P"   #'doom/open-private-config
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Recent project files"        "R"   #'projectile-recentf
        :desc "Save file"                   "s"   #'save-buffer
        :desc "Sudo find file"              "S"   #'doom/sudo-find-file
        :desc "Delete this file"            "X"   #'doom/delete-this-file
        :desc "Yank filename"               "y"   #'+default/yank-buffer-filename)

      ;;; <leader> g --- git
      (:prefix-map ("g" . "git")
        :desc "Git revert file"             "R"   #'vc-revert
        (:when (featurep! :ui vc-gutter)
          :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk)
        (:when (featurep! :tools magit)
          :desc "Magit dispatch"            "/"   #'magit-dispatch
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit status"              "g"   #'magit-status
          :desc "Magit file delete"         "x"   #'magit-file-delete
          :desc "Magit blame"               "B"   #'magit-blame-addition
          :desc "Magit clone"               "C"   #'+magit/clone
          :desc "Magit fetch"               "F"   #'magit-fetch
          :desc "Magit buffer log"          "L"   #'magit-log
          :desc "Git stage file"            "S"   #'magit-stage-file
          :desc "Git unstage file"          "U"   #'magit-unstage-file
          (:prefix ("f" . "find")
            :desc "Find file"                 "f"   #'magit-find-file
            :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
            :desc "Find commit"               "c"   #'magit-show-commit
            :desc "Find issue"                "i"   #'forge-visit-issue
            :desc "Find pull request"         "p"   #'forge-visit-pullreq)
          (:prefix ("o" . "open in browser")
            :desc "Browse region or line"     "."   #'+vc/git-browse-region-or-line
            :desc "Browse remote"             "r"   #'forge-browse-remote
            :desc "Browse commit"             "c"   #'forge-browse-commit
            :desc "Browse an issue"           "i"   #'forge-browse-issue
            :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
            :desc "Browse issues"             "I"   #'forge-browse-issues
            :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
          (:prefix ("l" . "list")
            (:when (featurep! :tools gist)
              :desc "List gists"              "g"   #'+gist:list)
            :desc "List repositories"         "r"   #'magit-list-repositories
            :desc "List submodules"           "s"   #'magit-list-submodules
            :desc "List issues"               "i"   #'forge-list-issues
            :desc "List pull requests"        "p"   #'forge-list-pullreqs
            :desc "List notifications"        "n"   #'forge-list-notifications)
          (:prefix ("c" . "create")
            :desc "Initialize repo"           "r"   #'magit-init
            :desc "Clone repo"                "R"   #'+magit/clone
            :desc "Commit"                    "c"   #'magit-commit-create
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
        :desc "Insert from clipboard"         "y"   #'+default/yank-pop
        :desc "Insert from evil register"     "r"   #'evil-ex-registers
        :desc "Insert snippet"                "s"   #'yas-insert-snippet)

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
        :desc "Browse notes"                  "." #'+default/browse-notes
        :desc "Search notes"                  "/" #'+default/org-notes-search
        :desc "Search notes for symbol"       "*" #'+default/search-notes-for-symbol-at-point
        :desc "Org capture"                   "c" #'org-capture
        :desc "Open deft"                     "d" #'deft
        :desc "Search org agenda headlines"   "h" #'+default/org-notes-headlines
        :desc "Find file in notes"            "n" #'+default/find-in-notes
        :desc "Browse notes"                  "N" #'+default/browse-notes
        :desc "Org store link"                "l" #'org-store-link)

      ;;; <leader> o --- open
      (:prefix-map ("o" . "open")
        :desc "Org agenda"       "A"  #'org-agenda
        (:prefix ("a" . "org agenda")
          :desc "Agenda"         "a"  #'org-agenda
          :desc "Todo list"      "t"  #'org-todo-list
          :desc "Tags search"    "m"  #'org-tags-view
          :desc "View search"    "v"  #'org-search-view)
        :desc "Default browser"    "b"  #'browse-url-of-file
        :desc "Debugger"           "d"  #'+debug/open
        :desc "REPL"               "r"  #'+eval/open-repl-other-window
        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
        :desc "Dired"              "-"  #'dired-jump
        (:when (featurep! :ui neotree)
          :desc "Project sidebar"              "p" #'+neotree/open
          :desc "Find file in project sidebar" "P" #'+neotree/find-this-file)
        (:when (featurep! :ui treemacs)
          :desc "Project sidebar" "p" #'+treemacs/toggle
          :desc "Find file in project sidebar" "P" #'+treemacs/find-file)
        (:when (featurep! :term term)
          :desc "Terminal"          "t" #'+term/open
          :desc "Terminal in popup" "T" #'+term/open-popup-in-project)
        (:when (featurep! :term vterm)
          :desc "Terminal"          "t" #'+vterm/open
          :desc "Terminal in popup" "T" #'+vterm/open-popup-in-project)
        (:when (featurep! :term eshell)
          :desc "Eshell"            "e" #'+eshell/open
          :desc "Eshell in popup"   "E" #'+eshell/open-popup)
        (:when (featurep! :collab floobits)
          (:prefix ("f" . "floobits")
            "c" #'floobits-clear-highlights
            "f" #'floobits-follow-user
            "j" #'floobits-join-workspace
            "l" #'floobits-leave-workspace
            "R" #'floobits-share-dir-private
            "s" #'floobits-summon
            "t" #'floobits-follow-mode-toggle
            "U" #'floobits-share-dir-public))
        (:when (featurep! :tools macos)
          :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
          :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
          :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
          :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
          :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
          :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar)
        (:when (featurep! :tools docker)
          :desc "Docker" "D" #'docker))

      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
        :desc "Browse project"               "." #'+default/browse-project
        :desc "Browse other project"         ">" #'doom/browse-in-other-project
        :desc "Find file in project"         "/" #'projectile-find-file
        :desc "Find file in other project"   "?" #'doom/find-file-in-other-project
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Add new project"              "a" #'projectile-add-known-project
        :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
        :desc "Compile in project"           "c" #'projectile-compile-project
        :desc "Remove known project"         "d" #'projectile-remove-known-project
        :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
        :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
        :desc "Kill project buffers"         "k" #'projectile-kill-buffers
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Find recent project files"    "r" #'projectile-recentf
        :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
        :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
        :desc "List project tasks"           "t" #'+default/project-tasks)

      ;;; <leader> q --- session
      (:prefix-map ("q" . "session")
        :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
        :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> r --- remote
      (:when (featurep! :tools upload)
        (:prefix-map ("r" . "remote")
          :desc "Upload local"               "u" #'ssh-deploy-upload-handler
          :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
          :desc "Download remote"            "d" #'ssh-deploy-download-handler
          :desc "Diff local & remote"        "D" #'ssh-deploy-diff-handler
          :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
          :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

      ;;; <leader> s --- snippets
      (:when (featurep! :editor snippets)
        (:prefix-map ("s" . "snippets")
          :desc "New snippet"                "n" #'yas-new-snippet
          :desc "Insert snippet"             "i" #'yas-insert-snippet
          :desc "Jump to mode snippet"       "/" #'yas-visit-snippet-file
          :desc "Jump to snippet"            "s" #'+snippets/find-file
          :desc "Browse snippets"            "S" #'+snippets/browse
          :desc "Reload snippets"            "r" #'yas-reload-all
          :desc "Create temporary snippet"   "c" #'aya-create
          :desc "Use temporary snippet"      "e" #'aya-expand))

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
        :desc "Flyspell"                     "s" #'flyspell-mode
        :desc "Flycheck"                     "f" #'flycheck-mode
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Indent guides"                "i" #'highlight-indent-guides-mode
        :desc "Impatient mode"               "h" #'+impatient-mode/toggle
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "org-tree-slide mode"          "p" #'+org-present/start))


;;
;;; Universal motion repeating keys

(defvar +default-repeat-keys (cons ";" ",")
  "The keys to use for repeating motions.

This is a cons cell whose CAR is the key for repeating a motion forward, and
whose CDR is for repeating backward. They should both be kbd-able strings.")

(when +default-repeat-keys
  (defmacro set-repeater! (command next-func prev-func)
    "Makes ; and , the universal repeat-keys in evil-mode.
To change these keys see `+default-repeat-keys'."
    (let ((fn-sym (intern (format "+default*repeat-%s" (doom-unquote command)))))
      `(progn
         (defun ,fn-sym (&rest _)
           (evil-define-key* 'motion 'local
             (kbd (car +default-repeat-keys)) #',next-func
             (kbd (cdr +default-repeat-keys)) #',prev-func))
         (advice-add #',command :after-while #',fn-sym))))

  ;; n/N
  (set-repeater! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  (set-repeater! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  (set-repeater! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  (set-repeater! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

  ;; f/F/t/T/s/S
  (after! evil-snipe
    (setq evil-snipe-repeat-keys nil
          evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;
    (set-repeater! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
    (set-repeater! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
    (set-repeater! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
    (set-repeater! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
    (set-repeater! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
    (set-repeater! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
    (set-repeater! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
    (set-repeater! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

  ;; */#
  (set-repeater! evil-visualstar/begin-search-forward
                 evil-ex-search-next evil-ex-search-previous)
  (set-repeater! evil-visualstar/begin-search-backward
                 evil-ex-search-previous evil-ex-search-next))


;;
;;; Universal evil integration

(when (featurep! :editor evil +everywhere)
  ;; Have C-u behave similarly to `doom/backward-to-bol-or-indent'.
  ;; NOTE SPC u replaces C-u as the universal argument.
  (map! :gi "C-u" #'doom/backward-kill-to-bol-and-indent
        :gi "C-w" #'backward-kill-word
        ;; Vimmish ex motion keys
        :gi "C-b" #'backward-word
        :gi "C-f" #'forward-word)

  (after! view
    (define-key view-mode-map [escape] #'View-quit-all))
  (after! man
    (evil-define-key* 'normal Man-mode-map "q" #'kill-current-buffer))

  ;; Minibuffer
  (define-key! evil-ex-completion-map
    "C-a" #'move-beginning-of-line
    "C-b" #'backward-word
    "C-s" (if (featurep! :completion ivy)
              #'counsel-minibuffer-history
            #'helm-minibuffer-history))

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-v"    #'yank
    "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
    "C-a"    #'move-beginning-of-line
    "C-b"    #'backward-word
    "C-r"    #'evil-paste-from-register
    ;; Scrolling lines
    "C-j"    #'next-line
    "C-k"    #'previous-line
    "C-S-j"  #'scroll-up-command
    "C-S-k"  #'scroll-down-command))
