;;; config/default/+emacs-bindings.el -*- lexical-binding: t; -*-

;; Sensible deafult key bindings for non-evil users
(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

;; persp-mode and projectile in different prefixes
(setq persp-keymap-prefix (kbd "C-c w"))
(after! projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;;
;;; Autoloads

(autoload 'org-capture-goto-target "org-capture" nil t)


;;
;;; Leader keys

(map! :leader
      :desc "Evaluate line/region"        "e"   #'+eval/line-or-region

      (:prefix ("l" . "<localleader>")) ; bound locally
      (:prefix ("!" . "checkers"))      ; bound by flycheck

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
        :desc "Compile"                               "c"   #'compile
        :desc "Recompile"                             "C"   #'recompile
        :desc "Jump to definition"                    "d"   #'+lookup/definition
        :desc "Jump to references"                    "D"   #'+lookup/references
        :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
        :desc "Evaluate & replace region"             "E"   #'+eval/region-and-replace
        :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
        :desc "Jump to documentation"                 "k"   #'+lookup/documentation
        :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
        :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
        :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
        :desc "List errors"                           "x"   #'flymake-show-diagnostics-buffer
        (:when (featurep! :tools flycheck)
          :desc "List errors"                         "x"   #'flycheck-list-errors)
        (:when (featurep! :tools lsp)
        :desc "LSP Code actions"                      "a"   #'lsp-execute-code-action
        :desc "LSP Format buffer/region"              "F"   #'+default/lsp-format-region-or-buffer
        :desc "LSP Organize imports"                  "i"   #'lsp-organize-imports
        :desc "LSP Rename"                            "r"   #'lsp-rename
        (:when (featurep! :completion ivy)
          :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
          :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
        (:when (featurep! :completion helm)
          :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
          :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol)))

      ;;; <leader> f --- file
      (:prefix-map ("f" . "file")
        :desc "Copy this file"              "C"   #'doom/copy-this-file
        :desc "Find file in private config" "C"   #'doom/find-file-in-private-config
        :desc "Delete this file"            "D"   #'doom/delete-this-file
        :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
        :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
        :desc "Find file from here"         "f"   #'+default/find-file-under-here
        :desc "Rename/move this file"       "m"   #'doom/move-this-file
        :desc "Browse private config"       "p"   #'doom/open-private-config
        :desc "Browse private config"       "P"   #'doom/open-private-config
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Recent project files"        "R"   #'projectile-recentf
        :desc "Sudo this file"              "s"   #'doom/sudo-this-file
        :desc "Sudo find file"              "S"   #'doom/sudo-find-file
        :desc "Yank filename"               "y"   #'+default/yank-buffer-filename
        :desc "Open scratch buffer"         "x"   #'doom/open-scratch-buffer
        :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
        (:when (featurep! :tools editorconfig)
          :desc "Open project editorconfig"   "."   #'editorconfig-find-current-editorconfig))

      ;;; <leader> r --- remote
      (:when (featurep! :tools upload)
        (:prefix-map ("r" . "remote")
          :desc "Upload local"               "u" #'ssh-deploy-upload-handler
          :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
          :desc "Download remote"            "d" #'ssh-deploy-download-handler
          :desc "Diff local & remote"        "D" #'ssh-deploy-diff-handler
          :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
          :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
        :desc "Search project for symbol"    "." #'+default/search-project-for-symbol-at-point
        :desc "Search buffer"                "b" #'swiper
        :desc "Search current directory"     "d" #'+default/search-cwd
        :desc "Search other directory"       "D" #'+default/search-other-cwd
        :desc "Locate file"                  "f" #'locate
        :desc "Jump to symbol"               "i" #'imenu
        :desc "Jump to visible link"         "l" #'link-hint-open-link
        :desc "Jump to link"                 "L" #'ffap-menu
        :desc "Jump list"                    "j" #'evil-show-jumps
        :desc "Jump to mark"                 "m" #'evil-show-marks
        :desc "Search project"               "p" #'+default/search-project
        :desc "Search other project"         "P" #'+default/search-other-project
        :desc "Search buffer"                "s" #'swiper-isearch
        :desc "Search buffer for thing at point" "S" #'swiper-isearch-thing-at-point)

      ;;; <leader> g --- lookup
      (:when (featurep! :tools lookup)
        (:prefix-map ("g" . "lookup")
          "k" #'+lookup/documentation
          "d" #'+lookup/definition
          "D" #'+lookup/references
          "f" #'+lookup/file
          "o" #'+lookup/online-select
          "i" #'+lookup/in-docsets
          "I" #'+lookup/in-all-docsets))

      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
        :desc "Current file name"             "f"   #'+default/insert-file-path
        :desc "Current file path"             "F"   (λ!! #'+default/insert-file-path t)
        :desc "Snippet"                       "s"   #'yas-insert-snippet
        :desc "Unicode"                       "u"   #'unicode-chars-list-chars
        :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
        :desc "Search notes for symbol"        "." #'+default/search-notes-for-symbol-at-point
        :desc "Org agenda"                     "a" #'org-agenda
        (:when (featurep! :tools biblio)
          :desc "Bibliographic entries"        "b"
          (cond ((featurep! :completion ivy)   #'ivy-bibtex)
                ((featurep! :completion helm)  #'helm-bibtex)))

        :desc "Toggle org-clock"               "c" #'+org/toggle-clock
        :desc "Cancel org-clock"               "C" #'org-clock-cancel
        :desc "Open deft"                      "d" #'deft
        (:when (featurep! :lang org +noter)
          :desc "Org noter"                    "e" #'org-noter)
      
        :desc "Find file in notes"             "f" #'+default/find-in-notes
        :desc "Browse notes"                   "F" #'+default/browse-notes
        :desc "Org store link"                 "l" #'org-store-link
        :desc "Tags search"                    "m" #'org-tags-view
        :desc "Org capture"                    "n" #'org-capture
        :desc "Goto capture"                   "N" #'org-capture-goto-target
        :desc "Active org-clock"               "o" #'org-clock-goto
        :desc "Todo list"                      "t" #'org-todo-list
        :desc "Search notes"                   "s" #'+default/org-notes-search
        :desc "Search org agenda headlines"    "S" #'+default/org-notes-headlines
        :desc "View search"                    "v" #'org-search-view
        :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
        :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text
        (:when (featurep! :lang org +journal)
          (:prefix ("j" . "journal")
            :desc "New Entry"      "j" #'org-journal-new-entry
            :desc "Search Forever" "s" #'org-journal-search-forever))
        (:when (featurep! :lang org +roam)
          (:prefix ("r" . "roam")
            :desc "Switch to buffer" "b" #'org-roam-switch-to-buffer
            :desc "Org Roam Capture" "c" #'org-roam-capture
            :desc "Find file"        "f" #'org-roam-find-file
            :desc "Show graph"       "g" #'org-roam-graph
            :desc "Insert"           "i" #'org-roam-insert
            :desc "Org Roam"         "r" #'org-roam
            (:prefix ("d" . "by date")
              :desc "Arbitrary date" "d" #'org-roam-dailies-date
              :desc "Today"          "t" #'org-roam-dailies-today
              :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
              :desc "Yesterday"      "y" #'org-roam-dailies-yesterday))))

      ;;; <leader> o --- open
      "o" nil ; we need to unbind it first as Org claims this prefix
      (:prefix-map ("o" . "open")
        :desc "Browser"            "b"  #'browse-url-of-file
        :desc "Debugger"           "d"  #'+debugger/start
        :desc "New frame"          "f"  #'make-frame
        :desc "REPL"               "r"  #'+eval/open-repl-other-window
        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
        :desc "Dired"              "-"  #'dired-jump
        (:when (featurep! :ui neotree)
          :desc "Project sidebar"               "p" #'+neotree/open
          :desc "Find file in project sidebar"  "P" #'+neotree/find-this-file)
        (:when (featurep! :ui treemacs)
          :desc "Project sidebar"               "p" #'+treemacs/toggle
          :desc "Find file in project rsidebar" "P" #'+treemacs/find-file)
        (:when (featurep! :term shell)
          :desc "Toggle shell popup"            "t" #'+shell/toggle
          :desc "Open shell here"               "T" #'+shell/here)
        (:when (featurep! :term term)
          :desc "Toggle terminal popup"         "t" #'+term/toggle
          :desc "Open terminal here"            "T" #'+term/here)
        (:when (featurep! :term vterm)
          :desc "Toggle vterm popup"            "t" #'+vterm/toggle
          :desc "Open vterm here"               "T" #'+vterm/here)
        (:when (featurep! :term eshell)
          :desc "Toggle eshell popup"           "e" #'+eshell/toggle
          :desc "Open eshell here"              "E" #'+eshell/here)
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
      (:prefix ("p" . "project")
        :desc "Search project for symbol"   "." #'+default/search-project-for-symbol-at-point
        :desc "Find file in other project"  "F" #'doom/find-file-in-other-project
        :desc "Search project"              "s" #'+default/search-project
        :desc "List project tasks"          "t" #'magit-todos-list
        :desc "Open project scratch buffer" "x" #'doom/open-project-scratch-buffer
        :desc "Switch to project scratch buffer" "X" #'doom/switch-to-project-scratch-buffer
        ;; later expanded by projectile
        (:prefix ("4" . "in other window"))
        (:prefix ("5" . "in other frame")))

      ;;; <leader> q --- quit/restart
      (:prefix-map ("q" . "quit/restart")
        :desc "Restart emacs server"         "d" #'+default/restart-server
        :desc "Delete frame"                 "f" #'delete-frame
        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
        :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
        :desc "Quit Emacs"                   "q" #'kill-emacs
        :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> & --- snippets
      (:prefix-map ("&" . "snippets")
        :desc "New snippet"           "n" #'yas-new-snippet
        :desc "Insert snippet"        "i" #'yas-insert-snippet
        :desc "Find global snippet"   "/" #'yas-visit-snippet-file
        :desc "Reload snippets"       "r" #'yas-reload-all
        :desc "Create Temp Template"  "c" #'aya-create
        :desc "Use Temp Template"     "e" #'aya-expand)

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Flymake"                      "f" #'flymake-mode
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Indent style"                 "I" #'doom/toggle-indent-style
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        :desc "Word-wrap mode"               "w" #'+word-wrap-mode
        (:when (featurep! :tools flycheck)
          :desc "Flycheck"                   "f" #'flycheck-mode)
        (:when (featurep! :ui indent-guides)
          :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
        (:when (featurep! :lang org +present)
          :desc "org-tree-slide mode"        "p" #'+org-present/start)
        :desc "Read-only mode"               "r" #'read-only-mode
        (:when (featurep! :tools flyspell)
          :desc "Flyspell"                   "s" #'flyspell-mode)
        (:when (featurep! :lang org +pomodoro)
          :desc "Pomodoro timer"             "t" #'org-pomodoro)
        (:when (featurep! :ui zen)
          :desc "Zen mode"                   "z" #'writeroom-mode))

      ;;; <leader> v --- versioning
      (:prefix-map ("v" . "versioning")
        :desc "Git revert file"             "R"   #'vc-revert
        :desc "Kill link to remote"         "y"   #'browse-at-remote-kill
        :desc "Kill link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
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
          :desc "Magit clone"               "C"   #'magit-clone
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
            :desc "Browse file or region"     "."   #'browse-at-remote
            :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
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
            :desc "Clone repo"                "R"   #'magit-clone
            :desc "Commit"                    "c"   #'magit-commit-create
            :desc "Fixup"                     "f"   #'magit-commit-fixup
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> w --- workspaces/windows
      (:prefix-map ("w" . "workspaces/windows")
        :desc "Autosave session"             "a" #'doom/quicksave-session
        :desc "Display workspaces"           "d" #'+workspace/display
        :desc "Rename workspace"             "r" #'+workspace/rename
        :desc "Create workspace"             "c" #'+workspace/new
        :desc "Delete workspace"             "k" #'+workspace/delete
        :desc "Save session"                 "s" #'doom/save-session
        :desc "Save workspace"               "S" #'+workspace/save
        :desc "Load session"                 "l" #'doom/load-session
        :desc "Load last autosaved session"  "L" #'doom/quickload-session
        :desc "Switch to other workspace"    "o" #'+workspace/other
        :desc "Undo window config"           "u" #'winner-undo
        :desc "Redo window config"           "U" #'winner-redo
        :desc "Switch to left workspace"     "p" #'+workspace/switch-left
        :desc "Switch to right workspace"    "n" #'+workspace/switch-right
        :desc "Switch to"                    "w" #'+workspace/switch-to
        :desc "Switch to workspace 1"        "1" #'+workspace/switch-to-0
        :desc "Switch to workspace 2"        "2" #'+workspace/switch-to-1
        :desc "Switch to workspace 3"        "3" #'+workspace/switch-to-2
        :desc "Switch to workspace 4"        "4" #'+workspace/switch-to-3
        :desc "Switch to workspace 5"        "5" #'+workspace/switch-to-4
        :desc "Switch to workspace 6"        "6" #'+workspace/switch-to-5
        :desc "Switch to workspace 7"        "7" #'+workspace/switch-to-6
        :desc "Switch to workspace 8"        "8" #'+workspace/switch-to-7
        :desc "Switch to workspace 9"        "9" #'+workspace/switch-to-8
        :desc "Switch to last workspace"     "0" #'+workspace/switch-to-final)

      ;;; <leader> m --- multiple cursors
      (:when (featurep! :editor multiple-cursors)
        (:prefix-map ("m" . "multiple cursors")
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
      ;;; <leader> M --- mu4e
      (:when (featurep! :email mu4e)
        (:prefix-map ("M" . "mu4e")
          :desc "Open email app" "M" #'=mu4e
          :desc "Compose email"  "c" #'+mu4e/compose))

      ;;; <leader> I --- IRC
      (:when (featurep! :app irc)
        (:prefix-map ("I" . "irc")
          :desc "Open irc app"       "I" #'=irc
          :desc "Next unread buffer" "a" #'tracking-next-buffer
          :desc "Quit irc"           "q" #'+irc/quit
          :desc "Reconnect all"      "r" #'circe-reconnect-all
          :desc "Send message"       "s" #'+irc/send-message
          (:when (featurep! :completion ivy)
            :desc "Jump to channel"  "j" #'irc/ivy-jump-to-channel)))

      ;;; <leader> T --- twitter
      (:when (featurep! :app twitter)
        (:prefix-map ("T" . "twitter")
          :desc "Open twitter app" "T" #'=twitter
          :desc "Quit twitter"     "q" #'+twitter/quit
          :desc "Rerender twits"   "r" #'+twitter/rerender-all
          :desc "Ace link"         "l" #'+twitter/ace-link)))


;;
;;; Global & plugin keybinds

(map! "C-'" #'imenu

      ;;; Text scaling
      [C-mouse-4] #'text-scale-increase
      [C-mouse-5] #'text-scale-decrease
      [C-down-mouse-2] (λ! (text-scale-set 0))
      "M-+" #'doom/reset-font-size
      "M-=" #'doom/increase-font-size
      "M--" #'doom/decrease-font-size

      ;;; newlines
      [remap newline]  #'newline-and-indent
      "C-j"            #'+default/newline

      ;;; search
      (:when (featurep! :completion ivy)
        "C-S-s"        #'swiper
        "C-S-r"        #'ivy-resume)
      (:when (featurep! :completion helm)
        "C-S-s"        #'swiper-helm
        "C-S-r"        #'helm-resume)

      ;;; objed
      (:when (featurep! :editor objed +manual)
        "M-SPC"     #'objed-activate)

      ;;; buffer management
      "C-x b"       #'switch-to-buffer
      "C-x 4 b"     #'switch-to-buffer-other-window
      (:when (featurep! :ui workspaces)
        "C-x b"       #'persp-switch-to-buffer
        "C-x B"       #'switch-to-buffer
        "C-x 4 B"     #'switch-to-buffer-other-window
        (:when (featurep! :completion ivy)
          "C-x 4 b"   #'+ivy/switch-workspace-buffer-other-window))
      "C-x C-b"     #'ibuffer-list-buffers
      "C-x K"       #'doom/kill-this-buffer-in-all-windows

      ;;; company-mode
      "C-;" #'+company/complete
      (:after company
        :map company-active-map
        "C-o"        #'company-search-kill-others
        "C-n"        #'company-select-next
        "C-p"        #'company-select-previous
        "C-h"        #'company-quickhelp-manual-begin
        "C-S-h"      #'company-show-doc-buffer
        "C-s"        #'company-search-candidates
        "M-s"        #'company-filter-candidates
        [C-tab]      #'company-complete-common-or-cycle
        [tab]        #'company-complete-common-or-cycle
        [backtab]    #'company-select-previous
        "C-RET"      #'counsel-company
        :map company-search-map
        "C-n"        #'company-search-repeat-forward
        "C-p"        #'company-search-repeat-backward
        "C-s"        (λ! (company-search-abort) (company-filter-candidates)))

      ;;; ein notebooks
      (:after ein:notebook-multilang
        :map ein:notebook-multilang-mode-map
        "C-c h" #'+ein/hydra/body)

      ;;; expand-region
      "C-="  #'er/expand-region
      "C--"  #'er/contract-region

      ;;; flycheck
      (:after flycheck
        :map flycheck-error-list-mode-map
        "C-n" #'flycheck-error-list-next-error
        "C-p" #'flycheck-error-list-previous-error
        "RET" #'flycheck-error-list-goto-error)

      ;;; help and info
      (:after help-mode
        :map help-mode-map
        "o" #'link-hint-open-link
        ">" #'help-go-forward
        "<" #'help-go-back
        "n" #'forward-button
        "p" #'backward-button)
      (:after helpful
        :map helpful-mode-map
        "o" #'link-hint-open-link)
      (:after apropos
        :map apropos-mode-map
        "o" #'link-hint-open-link
        "n" #'forward-button
        "p" #'backward-button)
      (:after info
        :map Info-mode-map
        "o" #'link-hint-open-link)

      ;;; ivy & counsel
      (:when (featurep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          "TAB"   #'ivy-alt-done
          "C-g"   #'keyboard-escape-quit)
        (:after counsel
          :map counsel-ag-map
          "C-SPC" #'ivy-call-and-recenter ; preview
          "M-RET" #'+ivy/git-grep-other-window-action)
        "C-M-y"   #'counsel-yank-pop)

      ;;; neotree
      (:when (featurep! :ui neotree)
        "<f9>"    #'+neotree/open
        "<C-f9>"  #'+neotree/find-this-file
        (:after neotree
          :map neotree-mode-map
          "q"     #'neotree-hide
          "RET"   #'neotree-enter
          "SPC"   #'neotree-quick-look
          "v"     #'neotree-enter-vertical-split
          "s"     #'neotree-enter-horizontal-split
          "c"     #'neotree-create-node
          "D"     #'neotree-delete-node
          "g"     #'neotree-refresh
          "r"     #'neotree-rename-node
          "R"     #'neotree-refresh
          "h"     #'+neotree/collapse-or-up
          "l"     #'+neotree/expand-or-open
          "n"     #'neotree-next-line
          "p"     #'neotree-previous-line
          "N"     #'neotree-select-next-sibling-node
          "P"     #'neotree-select-previous-sibling-node))

      ;;; popups
      (:when (featurep! :ui popup)
        "C-x p"   #'+popup/other
        "C-`"     #'+popup/toggle
        "C-~"     #'+popup/raise)

      ;;; smartparens
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

      ;;; treemacs
      (:when (featurep! :ui treemacs)
        "<f9>"   #'+treemacs/toggle
        "<C-f9>" #'+treemacs/find-file))
