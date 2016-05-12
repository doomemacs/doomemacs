;;; my-commands.el

;; Emacs utilities
(evil-ex-define-cmd "echo"          'narf:echo)
(evil-ex-define-cmd "minor"         'helm-describe-modes) ; list minor modes
;; Quick mapping keys to commands, allows :nmap \m !make
(evil-ex-define-cmd "nmap"          'narf:nmap)
(evil-ex-define-cmd "imap"          'narf:imap)
(evil-ex-define-cmd "vmap"          'narf:vmap)
(evil-ex-define-cmd "mmap"          'narf:mmap)
(evil-ex-define-cmd "omap"          'narf:omap)

;; Editing
(evil-ex-define-cmd "@"             'narf/evil-macro-on-all-lines) ; run macro on each line
(evil-ex-define-cmd "al[ign]"       'narf:align) ; align by regexp
(evil-ex-define-cmd "na[rrow]"      'narf:narrow) ; narrow buffer to selection
(evil-ex-define-cmd "ref[actor]"    'emr-show-refactor-menu) ; open emr menu
(evil-ex-define-cmd "retab"         'narf:whitespace-retab)
(evil-ex-define-cmd "settr[im]"     'narf:toggle-delete-trailing-whitespace)
(evil-ex-define-cmd "snip[pets]"    'narf:yas-snippets) ; visit a snippet
(evil-ex-define-cmd "tsnip[pets]"   'narf:yas-file-templates) ; visit a file template
(evil-ex-define-cmd "wal[ign]"      'narf:whitespace-align) ; align spaces
(evil-ex-define-cmd "rec[ent]"      'narf:helm-recentf) ; show recent files in helm
(evil-ex-define-cmd "reo[rient]"    'narf/window-reorient) ; scroll all windows to left
(evil-ex-define-cmd "ie[dit]"       'evil-multiedit-ex-match)
(evil-ex-define-cmd "htmle[nt]"     'narf/html-entities) ; encode/decode html entities

;; External resources
(evil-ex-define-cmd "dash"          'narf:dash) ; look up in Dash.app
(evil-ex-define-cmd "http"          'httpd-start) ; start http server
(evil-ex-define-cmd "re[gex]"       'narf:regex) ; open re-builder
(evil-ex-define-cmd "repl"          'narf:repl) ; invoke or send to repl
(evil-ex-define-cmd "t[mux]"        'narf:tmux) ; send to tmux
(evil-ex-define-cmd "tcd"           'narf:tmux-cd) ; cd to default-directory in tmux
(evil-ex-define-cmd "x"             'narf:send-to-scratch-or-org)
;; GIT
(evil-ex-define-cmd "gbr[owse]"     'narf:git-remote-browse) ; show file in github/gitlab

;; Dealing with buffers
(evil-ex-define-cmd "k[ill]"        'narf/kill-real-buffer)     ; Kill current buffer
(evil-ex-define-cmd "k[ill]all"     'narf:kill-all-buffers)     ; Kill all buffers (bang = in project)
(evil-ex-define-cmd "k[ill]buried"  'narf:kill-buried-buffers)  ; Kill all buried buffers (bang = in project)
(evil-ex-define-cmd "k[ill]o"       'narf:kill-other-buffers)   ; kill all other buffers
(evil-ex-define-cmd "k[ill]unreal"  'narf/kill-unreal-buffers)  ; kill unreal buffers
(evil-ex-define-cmd "k[ill]match"   'narf:kill-matching-buffers) ; kill buffers that match regexp
(evil-ex-define-cmd "l[ast]"        'narf/popup-last-buffer) ; pop up last popup
(evil-ex-define-cmd "m[sg]"         'narf/popup-messages) ; open *messages* in popup

;; Project navigation
(evil-ex-define-cmd "a"             'helm-projectile-find-other-file) ; open alternate file
(evil-ex-define-cmd "ag"            'narf:helm-ag-search) ; project text search
(evil-ex-define-cmd "ag[cw]d"       'narf:helm-ag-search-cwd) ; current directory search
(evil-ex-define-cmd "cd"            'narf:cd)
(evil-ex-define-cmd "se[arch]"      'narf:helm-swoop) ; in-file search
;; Project tools
(evil-ex-define-cmd "ma[ke]"        'narf:build)
(evil-ex-define-cmd "build"         'narf:build)
;; File operations
(evil-ex-define-cmd "mv"            'narf:file-move)
(evil-ex-define-cmd "rm"            'narf:file-delete)          ; rm[!]

;; Presentation/demo
(evil-ex-define-cmd "big"           'big-mode)
(evil-ex-define-cmd "full[scr]"     'narf:toggle-fullscreen)

;; Sessions/tabs
(evil-ex-define-cmd "sl[oad]"       'narf:load-session)
(evil-ex-define-cmd "ss[ave]"       'narf:save-session)
(evil-ex-define-cmd "tabs"          'narf/tab-display)
(evil-ex-define-cmd "tabn[ew]"      'narf:tab-create)
(evil-ex-define-cmd "tabr[ename]"   'narf:tab-rename)
(evil-ex-define-cmd "tabc[lose]"    'narf:kill-tab)
(evil-ex-define-cmd "tabc[lose]o"   'narf:kill-other-tabs)
(evil-ex-define-cmd "tabn[ext]"     'narf:switch-to-tab-right)
(evil-ex-define-cmd "tabp[rev]"     'narf:switch-to-tab-left)
(evil-ex-define-cmd "tabl[ast]"     'narf:switch-to-tab-last)

;; Org-mode
(evil-ex-define-cmd "link"          'narf:org-link)
(evil-ex-define-cmd "att[ach]"      'narf:org-attach) ; attach file to org file
(evil-ex-define-cmd "org"           'narf:org-helm-search) ; search org notes

;; Plugins
(after! flycheck
  (evil-ex-define-cmd "er[rors]"    (λ! (flycheck-buffer) (flycheck-list-errors))))

;; Debuggers
(evil-ex-define-cmd "debug"    'narf:debug)

(provide 'my-commands)
;;; my-commands.el ends here
