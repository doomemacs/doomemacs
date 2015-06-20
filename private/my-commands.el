;;; my-commands.el

(defalias 'exmap 'evil-ex-define-cmd)

(exmap "a"               'helm-projectile-find-other-file)
(exmap "acomp[ile]"      'narf:compile-autoloads)
(exmap "ag"              'narf:helm-search)
(exmap "ag[cw]d"         'narf:helm-search-cwd)
(exmap "agr"             'narf:helm-regex-search)
(exmap "agr[cw]d"        'narf:helm-regex-search-cwd)
(exmap "al[ign]"         'narf:whitespace-align)
(exmap "bcomp[ile]"      'narf:compile-el)
(exmap "cap[ture]"       'helm-org-capture-templates)
(exmap "cd"              'narf:cd)
(exmap "echo"            'narf:echo)
(exmap "en[ew]"          'narf:file-create)
(exmap "full[scr]"       'narf:toggle-fullscreen)
(exmap "ini"             'narf:ido-find-file-in-emacsd)
(exmap "k[ill]"          'kill-this-buffer)         ; Kill current buffer
(exmap "k[ill]all"       'narf:kill-all-buffers)    ; Kill all buffers (bang = in project)
(exmap "k[ill]buried"    'narf:kill-buried-buffers) ; Kill all buried buffers (bang = in project)
(exmap "k[ill]o"         'narf:kill-unreal-buffers)
(exmap "l[ast]"          'popwin:popup-last-buffer)
(exmap "m[sg]"           'popwin:messages)
(exmap "ma[ke]"          'narf:build)
(exmap "mv"              'narf:file-move)
(exmap "n[otes]"         'narf:org-search-files-or-headers)
(exmap "na[rrow]"        'narf:narrow)  ; Narrow buffer to selection
(exmap "proj[ect]"       'helm-projectile-switch-project)
(exmap "rec[ent]"        'narf:helm-recentf)
(exmap "ref[actor]"      'emr-show-refactor-menu)
(exmap "retab"           'narf:whitespace-retab)
(exmap "rm"              'narf:file-delete)          ; rm[!]
(exmap "settr[im]"       'narf:toggle-delete-trailing-whitespace)
(exmap "snip[pets]"      'narf:yas-snippets)         ; snip[!]
(exmap "sw[oop]"         'narf:helm-swoop)
(exmap "t"               'narf:tmux-run)
(exmap "tcd"             'narf:tmux-chdir)
(exmap "tsnip[pets]"     'narf:yas-file-templates)   ; temp[!]
;; (exmap "term"            'narf:term-init)
(exmap "x"               'narf:scratch-buffer)
(after! flycheck
  (exmap "er[rors]"      (λ (flycheck-buffer) (flycheck-list-errors))))
(after! re-builder
  (exmap "re[gex]"       'narf:regex))  ; TODO: Implement this
(after! org
  (exmap "o[rg]edit"     'org-edit-special)
  (exmap "o[rg]refile"   'org-refile)
  (exmap "o[rg]archive"  'org-archive-subtree)
  (exmap "o[rg]agenda"   'org-agenda)
  (exmap "o[rg]todo"     'org-show-todo-tree)
  (exmap "o[rg]link"     'org-link)
  (exmap "o[rg]align"    'org-align-all-tags))
(after! workgroups2
  (exmap "sl[oad]"       'narf:load-session)
  (exmap "ss[ave]"       'narf:save-session)
  (exmap "wg"            (λ (message (wg-workgroup-list-display))))
  (exmap "tabnew"        'narf:workgroup-new)
  (exmap "tabre[name]"   'narf:workgroup-rename)
  (exmap "tabn[ext]"     'wg-switch-to-workgroup-right)
  (exmap "tabp[rev]"     'wg-switch-to-workgroup-left)
  (exmap "tabl[ast]"     'wg-switch-to-previous-workgroup)
  (exmap "k[ill]w"       'wg-kill-workgroup)
  (exmap "k[ill]ow"      'narf:kill-other-workgroups))

(provide 'my-commands)
;;; my-commands.el ends here
