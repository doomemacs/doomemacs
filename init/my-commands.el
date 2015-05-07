(defalias 'exmap 'evil-ex-define-cmd)

(exmap "full[scr]"       'toggle-frame-fullscreen)
(exmap "ini"             'my:init-files)
(exmap "n[otes]"         'my:notes)
(exmap "recompile"       'my:byte-compile)
(exmap "cd"              'my:cd)
(exmap "en[ew]"          'my:create-file)
(exmap "ren[ame]"        'my:rename-this-file) ; Rename file . Bang: Delete old one
(exmap "al[ign]"         'my:align)
(exmap "retab"           'my:retab)
(exmap "sq[uint]"        'my:narrow-indirect)  ; Narrow buffer to selection
(exmap "x"               'my:scratch-buffer)

(exmap "k[ill]"          'kill-this-buffer)      ; Kill current buffer
(exmap "k[ill]o"         'my-cleanup-buffers)   ; Kill current project buffers
(exmap "k[ill]all"       'my:kill-buffers)    ; Kill all buffers (bang = project buffers only)
(exmap "k[ill]buried"    'my:kill-buried-buffers)    ; Kill all buffers (bang = project buffers only)

(exmap "ma[ke]"          'my:build)

(exmap "t"               'my:tmux-run)
(exmap "tcd"             'my:tmux-chdir)

(after "flycheck"
  (exmap "er[rors]"      (λ (flycheck-buffer) (flycheck-list-errors))))

(after "git-gutter-fringe+"
  (exmap "gstage"        'git-gutter+-stage-hunks)
  (exmap "grevert"       'git-gutter+-revert-hunks)
  (exmap "gdiff"         'git-gutter+-show-hunk))

(after "helm"
  (exmap "a"             'helm-projectile-find-other-file)
  (exmap "proj[ect]"     'helm-projectile-switch-project)
  (exmap "ag"            'my:helm-ag-search)
  (exmap "agr"           'my:helm-ag-regex-search)
  (exmap "ag[cw]d"       'my:helm-ag-search-cwd)
  (exmap "agr[cw]d"      'my:helm-ag-regex-search-cwd)
  (exmap "sw[oop]"       'my:helm-swoop)
  (exmap "rec[ent]"      'my:helm-recentf))

(after "yasnippet"
  (exmap "snip[pets]"    'ex:snippets))

(after "emr"
  (exmap "ref[actor]"    'emr-show-refactor-menu))

(after "re-builder"
  (exmap "re[gex]"       'my:regex))

(after "workgroups2"
  (exmap "sl[oad]"       'my:load-session)
  (exmap "ss[ave]"       'my:save-session)
  (exmap "wg"            (λ (message (wg-workgroup-list-display))))
  (exmap "wnew"          'my:new-workgroup)
  (exmap "wre[name]"     'my:rename-workgroup)
  (exmap "wn[ext]"       'wg-switch-to-workgroup-right)
  (exmap "wp[rev]"       'wg-switch-to-workgroup-left)
  (exmap "wl[ast]"       'wg-switch-to-previous-workgroup)
  (exmap "k[ill]w"       'wg-kill-workgroup)
  (exmap "k[ill]ow"      (λ (let (workgroup (wg-current-workgroup))
                              (dolist (w (wg-workgroup-list))
                                (unless (wg-current-workgroup-p w)
                                  (wg-kill-workgroup w)))))))

(after "org"
  (exmap "o[rg]edit"     'org-edit-special)
  (exmap "o[rg]refile"   'org-refile)
  (exmap "o[rg]archive"  'org-archive-subtree)
  (exmap "o[rg]agenda"   'org-agenda)
  (exmap "o[rg]todo"     'org-show-todo-tree)
  (exmap "o[rg]link"     'org-link)
  (exmap "o[rg]align"    'org-align-all-tags)
  (exmap "o[rg]image"    'my:org-insert-image))


(provide 'my-commands)
;;; my-commands.el ends here
