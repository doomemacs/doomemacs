;;; my-commands.el

;; Emacs utilities
(exmap "acomp[ile]"      'narf:compile-autoloads)
(exmap "bcomp[ile]"      'narf:compile-el)
(exmap "echo"            'narf:echo)
(exmap "minor"           'what-minor-modes)

;; Editing
(exmap "@"               'narf/evil-macro-on-all-lines)
(exmap "al[ign]"         'narf:align)
(exmap "en[ew]"          'narf:file-create)
(exmap "na[rrow]"        'narf:narrow)               ; Narrow buffer to selection
(exmap "ref[actor]"      'emr-show-refactor-menu)
(exmap "retab"           'narf:whitespace-retab)
(exmap "settr[im]"       'narf:toggle-delete-trailing-whitespace)
(exmap "snip[pets]"      'narf:yas-snippets)         ; snip[!]
(exmap "tsnip[pets]"     'narf:yas-file-templates)   ; tsnip[!]
(exmap "wal[ign]"        'narf:whitespace-align)
(exmap "rec[ent]"        'narf:helm-recentf)
(exmap "reo[rient]"      'narf/window-reorient)
(exmap "cols"            'narf:set-columns)

;; External resources
(exmap "dash"            'dash-at-point)
(exmap "http"            'httpd-start)
(exmap "re[gex]"         'narf:regex)
(exmap "repl"            'narf:repl)
(exmap "t[mux]"          'narf:tmux)
(exmap "t[mux]w"         'narf/tmux-new-window)
(exmap "tcd"             'narf:tmux-cd)
(exmap "x"               'narf:send-to-scratch-or-org)
;; GIT
(exmap "br[owse]"        'narf:git-remote-browse)

;; Dealing with buffers
(exmap "k[ill]"          'narf/kill-real-buffer)     ; Kill current buffer
(exmap "k[ill]all"       'narf:kill-all-buffers)     ; Kill all buffers (bang = in project)
(exmap "k[ill]buried"    'narf:kill-buried-buffers)  ; Kill all buried buffers (bang = in project)
(exmap "k[ill]o"         'narf:kill-unreal-buffers)
(exmap "k[ill]match"     'narf:kill-matching-buffers)
(exmap "l[ast]"          'narf/popup-last-buffer)
(exmap "m[sg]"           'narf/popup-messages)

;; Project navigation
(exmap "a"               'helm-projectile-find-other-file)
(exmap "ag"              'narf:helm-ag-search)
(exmap "ag[cw]d"         'narf:helm-ag-search-cwd)
(exmap "agr"             'narf:helm-ag-regex-search)
(exmap "agr[cw]d"        'narf:helm-ag-regex-search-cwd)
(exmap "cd"              'narf:cd)
(exmap "f[ind]"          'narf:helm-swoop)
;; Project tools
(exmap "ma[ke]"          'narf:build)
;; File operations
(exmap "mv"              'narf:file-move)
(exmap "rm"              'narf:file-delete)          ; rm[!]

;; Presentation/demo
(exmap "big"             'narf:toggle-big-mode)
(exmap "full[scr]"       'narf:toggle-fullscreen)
(exmap "fullw[rite]"     'narf:toggle-write-mode)

;; Org-mode
(exmap "cap[ture]"       'helm-org-capture-templates)
(exmap "org"             'narf/helm-org)
(exmap "cont[act]"       'narf:org-crm-contact)
(exmap "proj[ect]"       'narf:org-crm-project)
(exmap "invo[ice]"       'narf:org-crm-invoice)

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

;; Plugins
(after! flycheck
  (exmap "er[rors]"      (λ! (flycheck-buffer) (flycheck-list-errors))))

(provide 'my-commands)
;;; my-commands.el ends here
