;;; my-commands.el

(evil-define-operator narf:evil-ex-global (beg end pattern command &optional invert)
  "Rewritten :g[lobal] that will highlight buffer matches. Takes the same arguments."
  :motion mark-whole-buffer :move-point nil
  (interactive "<r><g//><!>")
  (evil-ex-global beg end pattern command invert))

(evil-define-operator narf:align (&optional beg end bang pattern)
  "Ex interface to `align-regexp'. Accepts vim-style regexps."
  (interactive "<r><!><//>")
  (align-regexp
   beg end
   (concat "\\(\\s-*\\)"
           (if bang
               (regexp-quote pattern)
             (evil-transform-vim-style-regexp pattern)))
   1 1))

;;; Rewritten commands
(ex! "g[lobal]"    'narf:evil-ex-global)

;;; Custom commands
;; Emacs utilities
(ex! "echo"        'narf:echo)
(ex! "minor"       'helm-describe-modes)         ; list minor modes
;; Quick mapping keys to commands, allows :nmap \m !make
(ex! "imap"        'narf:imap)
(ex! "mmap"        'narf:mmap)
(ex! "nmap"        'narf:nmap)
(ex! "omap"        'narf:omap)
(ex! "vmap"        'narf:vmap)
;; Editing
(ex! "@"           'narf/evil-macro-on-all-lines)
(ex! "al[ign]"     'narf:align)
(ex! "htmle[nt]"   'narf/html-entities)          ; encode/decode html entities
(ex! "ie[dit]"     'evil-multiedit-ex-match)
(ex! "na[rrow]"    'narf:narrow)
(ex! "rec[ent]"    'narf:helm-recentf)           ; show recent files
(ex! "ref[actor]"  'emr-show-refactor-menu)
(ex! "reo[rient]"  'narf/window-reorient)        ; scroll all windows to left
(ex! "retab"       'narf:whitespace-retab)
(ex! "settr[im]"   'narf:toggle-delete-trailing-whitespace)
(ex! "snip[pets]"  'narf:yas-snippets)           ; open snippet
(ex! "tsnip[pets]" 'narf:yas-file-templates)     ; open file template
(ex! "wal[ign]"    'narf:whitespace-align)       ; align by spaces
;; External resources
(ex! "dash"        'narf:dash)                   ; look up in Dash.app
(ex! "http"        'httpd-start)                 ; start http server
(ex! "re[gex]"     'narf:regex)                  ; open re-builder
(ex! "repl"        'narf:repl)                   ; invoke or send to repl
(ex! "t[mux]"      'narf:tmux)                   ; send to tmux
(ex! "tcd"         'narf:tmux-cd)                ; cd to default-directory in tmux
(ex! "x"           'narf:send-to-scratch-or-org)
;; GIT
(ex! "ga[dd]"      'narf/vcs-stage-hunk)
(ex! "gbr[owse]"   'narf:git-remote-browse)      ; show file in github/gitlab
(ex! "gre[vert]"   'narf/vcs-revert-hunk)
;; Dealing with buffers
(ex! "k[ill]"      'narf/kill-real-buffer)       ; Kill current buffer
(ex! "k[ill]all"   'narf:kill-all-buffers)       ; Kill buffers (bang = in project)
(ex! "k[ill]b"     'narf:kill-buried-buffers)    ; Kill buried buffers
(ex! "k[ill]m"     'narf:kill-matching-buffers)  ; kill buffers by regexp
(ex! "k[ill]o"     'narf:kill-other-buffers)     ; kill other buffers
(ex! "k[ill]u"     'narf/kill-unreal-buffers)    ; kill unreal buffers
(ex! "l[ast]"      'narf/popup-last-buffer)      ; pop up last popup
(ex! "m[sg]"       'narf/popup-messages)         ; open *messages* in popup
;; Project navigation
(ex! "a"           'helm-projectile-find-other-file)
(ex! "ag"          'narf:helm-ag-search)
(ex! "ag[cw]d"     'narf:helm-ag-search-cwd)
(ex! "cd"          'narf:cd)
(ex! "se[arch]"    'narf:helm-swoop)             ; in-file search
;; Project tools
(ex! "build"       'narf:build)
(ex! "ma[ke]"      'narf:build)
;; File operations
(ex! "mv"          'narf:file-move)
(ex! "rm"          'narf:file-delete)
;; Presentation/demo
(ex! "big"         'big-mode)
(ex! "full[scr]"   'narf:toggle-fullscreen)
;; Sessions/tabs
(ex! "sl[oad]"     'narf:load-session)
(ex! "ss[ave]"     'narf:save-session)
(ex! "tabc[lose]"  'narf:kill-tab)
(ex! "tabc[lose]o" 'narf:kill-other-tabs)
(ex! "tabl[ast]"   'narf:switch-to-tab-last)
(ex! "tabn[ew]"    'narf:tab-create)
(ex! "tabn[ext]"   'narf:switch-to-tab-right)
(ex! "tabp[rev]"   'narf:switch-to-tab-left)
(ex! "tabr[ename]" 'narf:tab-rename)
(ex! "tabs"        'narf/tab-display)
;; Org-mode
(ex! "att[ach]"    'narf:org-attach)             ; attach file to org file
(ex! "link"        'narf:org-link)
(ex! "org"         'narf:org-helm-search)        ; search org notes
;; Plugins
(ex! "er[rors]"    'narf/flycheck-errors)
;; Debuggers
(ex! "debug"       'narf:debug)

(provide 'my-commands)
;;; my-commands.el ends here
