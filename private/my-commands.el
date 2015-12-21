;;; my-commands.el

;; Emacs utilities
(exmap "acomp[ile]"      'narf:compile-autoloads)
(exmap "bcomp[ile]"      'narf:compile-el)
(exmap "echo"            'narf:echo)

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

;; External resources
(exmap "dash"            'dash-at-point)
(exmap "http"            'httpd-start)
(exmap "re[gex]"         'narf:regex)
(exmap "repl"            'narf:repl)
(exmap "t[mux]"          'narf:send-to-tmux)
(exmap "t[mux]s"         'narf/tmux-split-window)
(exmap "t[mux]v"         (λ! (narf/tmux-split-window t)))
(exmap "t[mux]w"         'narf/tmux-new-window)
(exmap "tcd"             'narf:tmux-cd)
(exmap "x"               'narf:scratch-buffer)
;; GIT
(exmap "git[hub]"        'narf:github-browse-file)

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
(exmap "fi[nd]"          'narf:helm-swoop)
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

;; Plugins
(after! flycheck
  (exmap "er[rors]"      (λ! (flycheck-buffer) (flycheck-list-errors))))

(after! workgroups2
  (exmap "sl[oad]"       'narf:load-session)
  (exmap "ss[ave]"       'narf:save-session)
  (exmap "wg"            'narf/workgroup-display)
  (exmap "tab"           'narf/workgroup-display)
  (exmap "tabn[ew]"      'narf:workgroup-new)
  (exmap "tabr[ename]"   'narf:workgroup-rename)
  (exmap "tabn[ext]"     'wg-switch-to-workgroup-right)
  (exmap "tabp[rev]"     'wg-switch-to-workgroup-left)
  (exmap "tabl[ast]"     'wg-switch-to-previous-workgroup)
  (exmap "tabq[uit]"     'narf:workgroup-delete)
  (exmap "k[ill]w"       'wg-kill-workgroup)
  (exmap "k[ill]ow"      'narf:kill-other-workgroups))

(provide 'my-commands)
;;; my-commands.el ends here
