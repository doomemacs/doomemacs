;;; my-commands.el

(defalias 'exmap 'evil-ex-define-cmd)
(defalias 'exmap! 'evil-ex-define-cmd-local)

(exmap "a"               'helm-projectile-find-other-file)
(exmap "acomp[ile]"      'narf:compile-autoloads)
(exmap "ag"              'narf:helm-ag-search)
(exmap "ag[cw]d"         'narf:helm-ag-search-cwd)
(exmap "agr"             'narf:helm-ag-regex-search)
(exmap "agr[cw]d"        'narf:helm-ag-regex-search-cwd)
(exmap "al[ign]"         'narf:whitespace-align)
(exmap "bcomp[ile]"      'narf:compile-el)
(exmap "big"             'narf:toggle-big-mode)
(exmap "cap[ture]"       'helm-org-capture-templates)
(exmap "cd"              'narf:cd)
(exmap "dash"            'dash-at-point)
(exmap "echo"            'narf:echo)
(exmap "en[ew]"          'narf:file-create)
(exmap "fi[nd]"          'narf:helm-swoop)
(exmap "full[scr]"       'narf:toggle-fullscreen)
(exmap "fullw[rite]"     'narf:toggle-write-mode)
(exmap "http"            'httpd-start)
(exmap "ini"             'narf:ido-find-file-in-emacsd)
(exmap "k[ill]"          'kill-this-buffer)          ; Kill current buffer
(exmap "k[ill]all"       'narf:kill-all-buffers)     ; Kill all buffers (bang = in project)
(exmap "k[ill]buried"    'narf:kill-buried-buffers)  ; Kill all buried buffers (bang = in project)
(exmap "k[ill]o"         'narf:kill-unreal-buffers)
(exmap "l[ast]"          'narf:popup-last-buffer)
(exmap "m[sg]"           'narf:popup-messages)
(exmap "ma[ke]"          'narf:build)
(exmap "mv"              'narf:file-move)
(exmap "na[rrow]"        'narf:narrow)               ; Narrow buffer to selection
(exmap "org"             'narf/helm-org)
(exmap "repl"            'narf:repl)
(exmap "proj[ect]"       'helm-projectile-switch-project)
(exmap "rec[ent]"        'narf:helm-recentf)
(exmap "re[gex]"         'narf:regex)
(exmap "ref[actor]"      'emr-show-refactor-menu)
(exmap "retab"           'narf:whitespace-retab)
(exmap "rm"              'narf:file-delete)          ; rm[!]
(exmap "settr[im]"       'narf:toggle-delete-trailing-whitespace)
(exmap "snip[pets]"      'narf:yas-snippets)         ; snip[!]
(exmap "tsnip[pets]"     'narf:yas-file-templates)   ; tsnip[!]
(exmap "x"               'narf:scratch-buffer)
(exmap "@"               'narf/evil-macro-on-all-lines)

(exmap "t[mux]"          'narf:send-to-tmux)
(exmap "t[mux]w"         'narf/tmux-new-window)
(exmap "t[mux]s"         'narf/tmux-split-window)
(exmap "t[mux]v"         (λ (narf/tmux-split-window t)))
(exmap "tcd"             'narf:tmux-cd)

(exmap "cont[act]"       'narf:org-crm-contact)
(exmap "proj[ect]"       'narf:org-crm-project)
(exmap "invo[ice]"       'narf:org-crm-invoice)

;; GIT
(exmap "bl[ame]"         'narf:github-browse-file)

(after! flycheck
  (exmap "er[rors]"      (λ (flycheck-buffer) (flycheck-list-errors))))

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
