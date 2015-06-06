;;; my-commands.el

(! (require 'core-defuns))

(@exmap "settr[im]"       'narf:toggle-delete-trailing-whitespace)

(@exmap "l[ast]"          'popwin:popup-last-buffer)
(@exmap "m[sg]"           'popwin:messages)

(@exmap "bcomp[ile]"      'narf:byte-compile)
(@exmap "acomp[ile]"      'narf:autoload-compile)

(@exmap "echo"            'narf:echo)
(@exmap "full[scr]"       'narf/toggle-fullscreen)
(@exmap "ini"             'narf:initfiles)
(@exmap "cd"              'narf:cd)
(@exmap "en[ew]"          'narf:create-file)
(@exmap "ren[ame]"        'narf:rename-this-file)          ; rename [NEWNAME]  # rename file
(@exmap "del[ete]"        'narf:delete-this-file)          ; delete[!]
(@exmap "al[ign]"         'narf:align)
(@exmap "retab"           'narf:retab)
(@exmap "na[rrow]"        'narf:narrow-indirect-or-widen)  ; Narrow buffer to selection
(@exmap "x"               'narf:scratch-buffer)
(@exmap "k[ill]"          'kill-this-buffer)                ; Kill current buffer
(@exmap "k[ill]o"         'narf:cleanup-buffers)            ; Kill current project buffers
(@exmap "k[ill]all"       'narf:kill-buffers)              ; Kill all buffers (bang = project buffers only)
(@exmap "k[ill]buried"    'narf:kill-buried-buffers)       ; Kill all buffers (bang = project buffers only)
(@exmap "ma[ke]"          'narf:build)
(@exmap "t"               'narf:tmux-run)
(@exmap "tcd"             'narf:tmux-chdir)
(@exmap "a"               'helm-projectile-find-other-file)
(@exmap "proj[ect]"       'helm-projectile-switch-project)
(@exmap "ag"              'narf:ag-search)
(@exmap "agr"             'narf:ag-regex-search)
(@exmap "ag[cw]d"         'narf:ag-search-cwd)
(@exmap "agr[cw]d"        'narf:ag-regex-search-cwd)
(@exmap "sw[oop]"         'narf:swoop)
(@exmap "rec[ent]"        'narf:recentf)
(@exmap "ref[actor]"      'emr-show-refactor-menu)
(@exmap "snip[pets]"      'narf::snippets)                  ; snip[!]
(@exmap "cap[ture]"       'helm-org-capture-templates)
(@exmap "n[otes]"         'helm-org-agenda-files-headings)
(after "flycheck"
  (@exmap "er[rors]"      (λ (flycheck-buffer) (flycheck-list-errors))))
(after "re-builder"
  (@exmap "re[gex]"       'narf::regex))
(after "org"
  (@exmap "o[rg]edit"     'org-edit-special)
  (@exmap "o[rg]refile"   'org-refile)
  (@exmap "o[rg]archive"  'org-archive-subtree)
  (@exmap "o[rg]agenda"   'org-agenda)
  (@exmap "o[rg]todo"     'org-show-todo-tree)
  (@exmap "o[rg]link"     'org-link)
  (@exmap "o[rg]align"    'org-align-all-tags))
(after "workgroups2"
  (@exmap "sl[oad]"       'narf:load-session)
  (@exmap "ss[ave]"       'narf:save-session)
  (@exmap "wg"            (λ (message (wg-workgroup-list-display))))
  (@exmap "wnew"          'narf:new-workgroup)
  (@exmap "wre[name]"     'narf:rename-workgroup)
  (@exmap "wn[ext]"       'wg-switch-to-workgroup-right)
  (@exmap "wp[rev]"       'wg-switch-to-workgroup-left)
  (@exmap "wl[ast]"       'wg-switch-to-previous-workgroup)
  (@exmap "k[ill]w"       'wg-kill-workgroup)
  (@exmap "k[ill]ow"      (λ (let (workgroup (wg-current-workgroup))
                              (dolist (w (wg-workgroup-list))
                                (unless (wg-current-workgroup-p w)
                                  (wg-kill-workgroup w)))))))


(provide 'my-commands)
;;; my-commands.el ends here
