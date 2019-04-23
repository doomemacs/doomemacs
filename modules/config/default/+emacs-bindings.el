;;; config/default/+emacs-bindings.el -*- lexical-binding: t; -*-

;; Sensible deafult key bindings for non-evil users
(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

;; persp-mode and projectile in different prefixes
(setq persp-keymap-prefix (kbd "C-c w"))
(after! projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(after! which-key
  (which-key-add-key-based-replacements "C-c !" "checking")
  (which-key-add-key-based-replacements "C-c l" "<localleader>"))


;;
;;; Global keybinds

(map! "C-'" #'imenu
      ;; Text scaling
      "<C-mouse-4>"      #'text-scale-increase
      "<C-mouse-5>"      #'text-scale-decrease
      "<C-down-mouse-2>" (λ! (text-scale-set 0))
      "M-+" (λ! (text-scale-set 0))
      "M-=" #'text-scale-increase
      "M--" #'text-scale-decrease
      ;; Editor related bindings
      [remap newline]  #'newline-and-indent
      "C-j"            #'+default/newline
      (:when (featurep! :completion ivy)
        "C-S-s"        #'swiper
        "C-S-r"        #'ivy-resume)
      (:when (featurep! :completion helm)
        "C-S-s"        #'swiper-helm
        "C-S-r"        #'helm-resume)
      ;; Buffer related bindings
      "C-x b"       #'persp-switch-to-buffer
      (:when (featurep! :completion ivy)
        "C-x 4 b"   #'+ivy/switch-workspace-buffer-other-window)
      "C-x C-b"     #'ibuffer-list-buffers
      "C-x B"       #'switch-to-buffer
      "C-x 4 B"     #'switch-to-buffer-other-window
      "C-x k"       #'doom/kill-this-buffer-in-all-windows
      ;; Popup bindigns
      "C-x p"   #'+popup/other
      "C-`"     #'+popup/toggle
      "C-~"     #'+popup/raise)


;;
;;; Leader keys

(map! :leader
      :desc "Find file in project"        "C-f" #'projectile-find-file
      :desc "Evaluate line/region"        "e"   #'+eval/line-or-region
      :desc "Open scratch buffer"         "x"   #'doom/open-scratch-buffer
      :desc "Open project scratch buffer" "X"   #'doom/open-project-scratch-buffer

      (:when (featurep! :emacs term)
        :desc "Terminal"              "`" #'+term/open
        :desc "Terminal in popup"     "~" #'+term/open-popup-in-project)
      (:when (featurep! :tools vterm)
        :desc "Terminal"              "`" #'+vterm/open
        :desc "Terminal in popup"     "~" #'+vterm/open-popup-in-project)
      (:when (featurep! :emacs eshell)
        :desc "Eshell"                "`" #'+eshell/open
        :desc "Eshell in popup"       "~" #'+eshell/open-popup)

      ;; Add labels to prefixes defined elsewhere
      :desc "project" "p" nil

      (:prefix ("f" . "file")
        :desc "Find other file"             "a"   #'projectile-find-other-file
        :desc "Browse private config"       "c"   #'doom/open-private-config
        :desc "Find file in private config" "C"   #'doom/find-file-in-private-config
        :desc "Open project editorconfig"   "."   #'editorconfig-find-current-editorconfig
        :desc "Find directory"              "d"   #'dired
        :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
        :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
        :desc "Find file from here"         "f"   (if (fboundp 'counsel-file-jump) #'counsel-file-jump #'find-file)
        :desc "Find file in other project"  "F"   #'doom/browse-in-other-project
        :desc "Find file in project"        "p"   #'projectile-find-file
        :desc "Find file in other project"  "P"   #'doom/find-file-in-other-project
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Recent project files"        "R"   #'projectile-recentf
        :desc "Sudo this file"              "s"   #'doom/sudo-this-file
        :desc "Sudo find file"              "S"   #'doom/sudo-find-file
        :desc "Delete this file"            "X"   #'doom/delete-this-file
        :desc "Yank filename"               "y"   #'+default/yank-buffer-filename)

      "o" nil ; we need to unbind it first as Org claims this
      (:prefix ("o". "org")
        (:prefix ("a" . "org agenda")
          :desc "Agenda"                  "a"   #'org-agenda
          :desc "Todo list"               "t"   #'org-todo-list
          :desc "Tags view"               "m"   #'org-tags-view
          :desc "View search"             "v"   #'org-search-view)
        :desc "Switch org buffers"      "b"     #'org-switchb
        :desc "Capture"                 "c"     #'org-capture
        :desc "Goto capture"            "C"     (λ! (require 'org-capture) (call-interactively #'org-capture-goto-target))
        :desc "Link store"              "l"     #'org-store-link
        :desc "Sync org caldav"         "s"     #'org-caldav-sync)

      (:prefix ("q" . "quit/restart")
        :desc "Quit Emacs"                   "q" #'kill-emacs
        :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
        (:when (featurep! :feature workspaces)
          :desc "Quit Emacs & forget session"  "X" #'+workspace/kill-session-and-quit)
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      (:prefix ("&" . "snippets")
        :desc "New snippet"           "n" #'yas-new-snippet
        :desc "Insert snippet"        "i" #'yas-insert-snippet
        :desc "Find global snippet"   "/" #'yas-visit-snippet-file
        :desc "Reload snippets"       "r" #'yas-reload-all
        :desc "Create Temp Template"  "c" #'aya-create
        :desc "Use Temp Template"     "e" #'aya-expand)

      (:prefix ("v" . "versioning")
        :desc "Git revert file"             "R"   #'vc-revert
        (:when (featurep! :ui vc-gutter)
          :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"         "n"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "p"   #'git-gutter:previous-hunk)
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

      (:prefix ("w" . "workspaces/windows")
        :desc "Autosave session"             "a" #'doom/quicksave-session
        :desc "Display workspaces"           "d" #'+workspace/display
        :desc "Rename workspace"             "r" #'+workspace/rename
        :desc "Create workspace"             "c" #'+workspace/new
        :desc "Delete workspace"             "k" #'+workspace/delete
        :desc "Save session"                 "s" #'doom/save-session
        :desc "Save workspace"               "S" #'+workspace/save
        :desc "Load session"                 "l" #'doom/load-session
        :desc "Load last autosaved session"  "L" #'doom/quickload-session
        :desc "Kill other buffers"           "o" #'doom/kill-other-buffers
        :desc "Undo window config"           "u" #'winner-undo
        :desc "Redo window config"           "U" #'winner-redo
        :desc "Switch to left workspace"     "p" #'+workspace/switch-left
        :desc "Switch to right workspace"    "n" #'+workspace/switch-right
        :desc "Switch to"                    "w" #'+workspace/switch-to
        :desc "Switch to workspace 1"        "1" (λ! (+workspace/switch-to 0))
        :desc "Switch to workspace 2"        "2" (λ! (+workspace/switch-to 1))
        :desc "Switch to workspace 3"        "3" (λ! (+workspace/switch-to 2))
        :desc "Switch to workspace 4"        "4" (λ! (+workspace/switch-to 3))
        :desc "Switch to workspace 5"        "5" (λ! (+workspace/switch-to 4))
        :desc "Switch to workspace 6"        "6" (λ! (+workspace/switch-to 5))
        :desc "Switch to workspace 7"        "7" (λ! (+workspace/switch-to 6))
        :desc "Switch to workspace 8"        "8" (λ! (+workspace/switch-to 7))
        :desc "Switch to workspace 9"        "9" (λ! (+workspace/switch-to 8))
        :desc "Switch to last workspace"     "0" #'+workspace/switch-to-last)

      (:when (featurep! :editor multiple-cursors)
        (:prefix ("m" . "multiple cursors")
          :desc "Edit lines"         "l"         #'mc/edit-lines
          :desc "Mark next"          "n"         #'mc/mark-next-like-this
          :desc "Unmark next"        "N"         #'mc/unmark-next-like-this
          :desc "Mark previous"      "p"         #'mc/mark-previous-like-this
          :desc "Unmark previous"    "P"         #'mc/unmark-previous-like-this
          :desc "Mark all"           "t"         #'mc/mark-all-like-this
          :desc "Mark all DWIM"      "m"         #'mc/mark-all-like-this-dwim
          :desc "Edit line endings"  "e"         #'mc/edit-ends-of-lines
          :desc "Edit line starts"   "a"         #'mc/edit-beginnings-of-lines
          :desc "Mark tag"           "s"         #'mc/mark-sgml-tag-pair
          :desc "Mark in defun"      "d"         #'mc/mark-all-like-this-in-defun
          :desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click))

      ;; APPs
      (:when (featurep! :app email)
        (:prefix ("M" . "email")
          :desc "Open email app" "M" #'=email
          :desc "Compose email"  "c" #'+email/compose))

      (:when (featurep! :app irc)
        (:prefix ("I" . "irc")
          :desc "Open irc app"       "I" #'=irc
          :desc "Next unread buffer" "a" #'tracking-next-buffer
          :desc "Quit irc"           "q" #'+irc/quit
          :desc "Reconnect all"      "r" #'circe-reconnect-all
          :desc "Send message"       "s" #'+irc/send-message
          (:when (featurep! :completion ivy)
            :desc "Jump to channel"  "j" #'irc/ivy-jump-to-channel)))

      (:when (featurep! :app twitter)
        (:prefix ("T" . "twitter")
          :desc "Open twitter app" "T" #'=twitter
          :desc "Quit twitter"     "q" #'+twitter/quit
          :desc "Rerender twits"   "r" #'+twitter/rerender-all
          :desc "Ace link"         "l" #'+twitter/ace-link)))


;;
;;; Plugins

(map! "C-="  #'er/expand-region
      "C--"  #'er/contract-region
      (:when (featurep! :ui neotree)
        "<f9>"   #'+neotree/open
        "<F-f9>" #'+neotree/find-this-file)
      (:when (featurep! :ui treemacs)
        "<f9>"   #'+treemacs/open
        "<F-f9>" #'+treemacs/find-file)
      ;; smartparens
      (:after smartparens
        :map smartparens-mode-map
        "C-M-a"     #'sp-beginning-of-sexp
        "C-M-e"     #'sp-end-of-sexp
        "C-M-f"     #'sp-forward-sexp
        "C-M-b"     #'sp-backward-sexp
        "C-M-d"     #'sp-splice-sexp
        "C-M-k"     #'sp-kill-sexp
        "C-M-t"     #'sp-transpose-sexp
        "C-<right>" #'sp-forward-slurp-sexp
        "M-<right>" #'sp-forward-barf-sexp
        "C-<left>"  #'sp-backward-slurp-sexp
        "M-<left>"  #'sp-backward-barf-sexp)
      ;; company mode
      "C-;" #'+company/complete
      ;; Counsel
      (:when (featurep! :completion ivy)
        (:after counsel
          :map counsel-ag-map
          "C-c C-e"  #'+ivy/wgrep-occur      ; search/replace on results
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action))
        "C-M-y" #'counsel-yank-pop)
      ;; repl toggle
      "C-c C-z" #'+eval/open-repl-other-window
      ;; company mode
      (:after company
        :map company-active-map
        "C-o"        #'company-search-kill-others
        "C-n"        #'company-select-next
        "C-p"        #'company-select-previous
        "C-h"        #'company-quickhelp-manual-begin
        "C-S-h"      #'company-show-doc-buffer
        "C-s"        #'company-search-candidates
        "M-s"        #'company-filter-candidates
        "<C-tab>"    #'company-complete-common-or-cycle
        [tab]        #'company-complete-common-or-cycle
        [backtab]    #'company-select-previous
        "C-RET"      #'counsel-company
        :map company-search-map
        "C-n"        #'company-search-repeat-forward
        "C-p"        #'company-search-repeat-backward
        "C-s"        (λ! (company-search-abort) (company-filter-candidates)))
      ;; neotree bindings
      (:after neotree
        :map neotree-mode-map
        "q"       #'neotree-hide
        "RET"     #'neotree-enter
        "SPC"     #'neotree-quick-look
        "v"       #'neotree-enter-vertical-split
        "s"       #'neotree-enter-horizontal-split
        "c"       #'neotree-create-node
        "D"       #'neotree-delete-node
        "g"       #'neotree-refresh
        "r"       #'neotree-rename-node
        "R"       #'neotree-refresh
        "h"       #'+neotree/collapse-or-up
        "l"       #'+neotree/expand-or-open
        "n"       #'neotree-next-line
        "p"       #'neotree-previous-line
        "N"       #'neotree-select-next-sibling-node
        "P"       #'neotree-select-previous-sibling-node)
      ;; help and info
      (:after help-mode
        :map help-mode-map
        "o" #'ace-link-help
        ">" #'help-go-forward
        "<" #'help-go-back
        "n" #'forward-button
        "p" #'backward-button)
      (:after helpful
        :map helpful-mode-map
        "o" #'ace-link-help)
      (:after apropos
        :map apropos-mode-map
        "o" #'ace-link-help
        "n" #'forward-button
        "p" #'backward-button)
      (:after info
        :map Info-mode-map
        "o" #'ace-link-info)
      ;; yasnippet
      (:after yasnippet
        ;; keymap while editing an inserted snippet
        :map yas-keymap
        "C-e"           #'+snippets/goto-end-of-field
        "C-a"           #'+snippets/goto-start-of-field
        "<S-tab>"       #'yas-prev-field
        "<M-backspace>" #'+snippets/delete-to-start-of-field
        [backspace]     #'+snippets/delete-backward-char
        [delete]        #'+snippets/delete-forward-char-or-field)
      ;; flycheck
      (:after flycheck
        :map flycheck-error-list-mode-map
        "C-n" #'flycheck-error-list-next-error
        "C-p" #'flycheck-error-list-previous-error
        "RET" #'flycheck-error-list-goto-error)
      ;; ivy
      (:after ivy
        :map ivy-minibuffer-map
        "TAB" #'ivy-alt-done
        "C-g" #'keyboard-escape-quit)
      ;; ein notebokks
      (:after ein:notebook-multilang
        :map ein:notebook-multilang-mode-map
        "C-c h" #'+ein/hydra/body))
