;;; my-commands.el

;;; Rewritten commands
(ex! "g[lobal]"    'doom:evil-ex-global)

;;; Custom commands
;; Emacs utilities
(ex! "echo"        'doom:echo)
(ex! "minor"       'helm-describe-modes)         ; list minor modes
;; Quick mapping keys to commands, allows :nmap \m !make
(ex! "imap"        'doom:imap)
(ex! "mmap"        'doom:mmap)
(ex! "nmap"        'doom:nmap)
(ex! "omap"        'doom:omap)
(ex! "vmap"        'doom:vmap)
;; Editing
(ex! "@"           'doom/evil-macro-on-all-lines)
(ex! "al[ign]"     'doom:align)
(ex! "htmle[nt]"   'doom/html-entities)          ; encode/decode html entities
(ex! "ie[dit]"     'evil-multiedit-ex-match)
(ex! "na[rrow]"    'doom:narrow)
(ex! "rec[ent]"    'doom:helm-recentf)           ; show recent files
(ex! "ref[actor]"  'emr-show-refactor-menu)
(ex! "reo[rient]"  'doom/window-reorient)        ; scroll all windows to left
(ex! "retab"       'doom:whitespace-retab)
(ex! "settr[im]"   'doom:toggle-delete-trailing-whitespace)
(ex! "snip[pets]"  'doom:yas-snippets)           ; open snippet
(ex! "tsnip[pets]" 'doom:yas-file-templates)     ; open file template
(ex! "wal[ign]"    'doom:whitespace-align)       ; align by spaces
;; External resources
(ex! "dash"        'doom:dash)                   ; look up in Dash.app
(ex! "http"        'httpd-start)                 ; start http server
(ex! "re[gex]"     'doom:regex)                  ; open re-builder
(ex! "repl"        'doom:repl)                   ; invoke or send to repl
(ex! "t[mux]"      'doom:tmux)                   ; send to tmux
(ex! "tcd"         'doom:tmux-cd)                ; cd to default-directory in tmux
(ex! "x"           'doom:send-to-scratch-or-org)
;; GIT
(ex! "ga[dd]"      'doom/vcs-stage-hunk)
(ex! "gbr[owse]"   'doom:git-remote-browse)      ; show file in github/gitlab
(ex! "gre[vert]"   'doom/vcs-revert-hunk)
;; Dealing with buffers
(ex! "k[ill]"      'doom/kill-real-buffer)       ; Kill current buffer
(ex! "k[ill]all"   'doom:kill-all-buffers)       ; Kill buffers (bang = in project)
(ex! "k[ill]b"     'doom:kill-buried-buffers)    ; Kill buried buffers
(ex! "k[ill]m"     'doom:kill-matching-buffers)  ; kill buffers by regexp
(ex! "k[ill]o"     'doom:kill-other-buffers)     ; kill other buffers
(ex! "k[ill]u"     'doom/kill-unreal-buffers)    ; kill unreal buffers
(ex! "l[ast]"      'doom/popup-last-buffer)      ; pop up last popup
(ex! "m[sg]"       'doom/popup-messages)         ; open *messages* in popup
;; Project navigation
(ex! "a"           'helm-projectile-find-other-file)
(ex! "ag"          'doom:helm-ag-search)
(ex! "ag[cw]d"     'doom:helm-ag-search-cwd)
(ex! "cd"          'doom:cd)
(ex! "se[arch]"    'doom:helm-swoop)             ; in-file search
;; Project tools
(ex! "build"       'doom:build)
(ex! "ma[ke]"      'doom:build)
;; File operations
(ex! "mv"          'doom:file-move)
(ex! "rm"          'doom:file-delete)
;; Presentation/demo
(ex! "big"         'big-mode)
(ex! "full[scr]"   'doom:toggle-fullscreen)
;; Sessions/tabs
(ex! "sl[oad]"     'doom:load-session)
(ex! "ss[ave]"     'doom:save-session)
(ex! "tabc[lose]"  'doom:kill-tab)
(ex! "tabc[lose]o" 'doom:kill-other-tabs)
(ex! "tabl[ast]"   'doom:switch-to-tab-last)
(ex! "tabn[ew]"    'doom:tab-create)
(ex! "tabn[ext]"   'doom:switch-to-tab-right)
(ex! "tabp[rev]"   'doom:switch-to-tab-left)
(ex! "tabr[ename]" 'doom:tab-rename)
(ex! "tabs"        'doom/tab-display)
;; Org-mode
(ex! "att[ach]"    'doom:org-attach)             ; attach file to org file
(ex! "link"        'doom:org-link)
(ex! "org"         'doom:org-helm-search)        ; search org notes
;; Plugins
(ex! "er[rors]"    'doom/flycheck-errors)
;; Debuggers
(ex! "debug"       'doom:debug)

(provide 'my-commands)
;;; my-commands.el ends here
