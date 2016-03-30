;;; my-commands.el

;; Emacs utilities
(exmap "acomp[ile]"    'narf:compile-autoloads) ; compile autoloads
(exmap "bcomp[ile]"    'narf:compile-el)
(exmap "echo"          'narf:echo)
(exmap "minor"         'helm-describe-modes) ; list minor modes

;; Editing
(exmap "@"             'narf/evil-macro-on-all-lines) ; run macro on each line
(exmap "al[ign]"       'narf:align) ; align by regexp
(exmap "na[rrow]"      'narf:narrow) ; narrow buffer to selection
(exmap "ref[actor]"    'emr-show-refactor-menu) ; open emr menu
(exmap "retab"         'narf:whitespace-retab)
(exmap "settr[im]"     'narf:toggle-delete-trailing-whitespace)
(exmap "snip[pets]"    'narf:yas-snippets) ; visit a snippet
(exmap "tsnip[pets]"   'narf:yas-file-templates) ; visit a file template
(exmap "wal[ign]"      'narf:whitespace-align) ; align spaces
(exmap "rec[ent]"      'narf:helm-recentf) ; show recent files in helm
(exmap "reo[rient]"    'narf/window-reorient) ; scroll all windows to left
(exmap "ie[dit]"       'evil-multiedit-ex-match)

;; External resources
(exmap "dash"          'narf:dash) ; look up in Dash.app
(exmap "http"          'httpd-start) ; start http server
(exmap "re[gex]"       'narf:regex) ; open re-builder
(exmap "repl"          'narf:repl) ; invoke or send to repl
(exmap "t[mux]"        'narf:tmux) ; send to tmux
(exmap "tcd"           'narf:tmux-cd) ; cd to default-directory in tmux
(exmap "x"             'narf:send-to-scratch-or-org)
;; GIT
(exmap "gbr[owse]"     'narf:git-remote-browse) ; show file in github/gitlab

;; Dealing with buffers
(exmap "k[ill]"        'narf/kill-real-buffer)     ; Kill current buffer
(exmap "k[ill]all"     'narf:kill-all-buffers)     ; Kill all buffers (bang = in project)
(exmap "k[ill]buried"  'narf:kill-buried-buffers)  ; Kill all buried buffers (bang = in project)
(exmap "k[ill]o"       'narf:kill-other-buffers)   ; kill all other buffers
(exmap "k[ill]unreal"  'narf/kill-unreal-buffers)  ; kill unreal buffers
(exmap "k[ill]match"   'narf:kill-matching-buffers) ; kill buffers that match regexp
(exmap "l[ast]"        'narf/popup-last-buffer) ; pop up last popup
(exmap "m[sg]"         'narf/popup-messages) ; open *messages* in popup

;; Project navigation
(exmap "a"             'helm-projectile-find-other-file) ; open alternate file
(exmap "ag"            'narf:helm-ag-search) ; project text search
(exmap "ag[cw]d"       'narf:helm-ag-search-cwd) ; current directory search
(exmap "cd"            'narf:cd)
(exmap "se[arch]"      'narf:helm-swoop) ; in-file search
;; Project tools
(exmap "ma[ke]"        'narf:build)
(exmap "build"         'narf:build)
;; File operations
(exmap "mv"            'narf:file-move)
(exmap "rm"            'narf:file-delete)          ; rm[!]

;; Presentation/demo
(exmap "big"           'big-mode)
(exmap "full[scr]"     'narf:toggle-fullscreen)

;; Sessions/tabs
(exmap "sl[oad]"       'narf:load-session)
(exmap "ss[ave]"       'narf:save-session)
(exmap "tabs"          'narf/tab-display)
(exmap "tabn[ew]"      'narf:tab-create)
(exmap "tabr[ename]"   'narf:tab-rename)
(exmap "tabc[lose]"    'narf:kill-tab)
(exmap "tabc[lose]o"   'narf:kill-other-tabs)
(exmap "tabn[ext]"     'narf:switch-to-tab-right)
(exmap "tabp[rev]"     'narf:switch-to-tab-left)
(exmap "tabl[ast]"     'narf:switch-to-tab-last)

;; Org-mode
(exmap "link"          'narf:org-link)
(exmap "att[ach]"      'narf:org-attach) ; attach file to org file
(exmap "org"           'narf:org-helm-search) ; search org notes

;; Plugins
(after! flycheck
  (exmap "er[rors]"    (λ! (flycheck-buffer) (flycheck-list-errors))))

(provide 'my-commands)
;;; my-commands.el ends here
