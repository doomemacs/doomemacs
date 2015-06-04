;; ex-commands
(evil-define-command narf::byte-compile (&optional bang)
  :repeat nil
  (interactive "<!>")
  (when emacs-lisp-mode
    (if (not bang)
        (progn ;; (byte-recompile-file (concat CORE-DIR "defuns.el") t 0)
               (byte-recompile-file (buffer-file-name) t 0))
      (byte-recompile-file (expand-file-name "init.el" BASE-DIR) nil 0)
      (byte-recompile-directory CORE-DIR 0 nil)
      (byte-recompile-directory CONTRIB-DIR 0 nil)
      (dolist (path (directory-files MODULES-DIR t "\\(core\\|defuns\\|narf\\).*\\.el$"))
        (byte-recompile-file path nil 0)))))

(evil-define-command narf::autoload-compile (&optional bang)
  :repeat nil
  (interactive "<!>")
  (defvar generated-autoload-file (expand-file-name "autoloads.el" MODULES-DIR))
  (update-directory-autoloads CORE-DIR MODULES-DIR CONTRIB-DIR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(excmd! "settr[im]"       'narf:toggle-delete-trailing-whitespace)

(excmd! "echo"            'narf::echo)
(excmd  "l[ast]"          'popwin:popup-last-buffer)
(excmd  "m[sg]"           'popwin:messages)
(excmd! "full[scr]"       'narf:toggle-fullscreen)
(excmd! "ini"             'narf::initfiles)
(excmd! "bcomp[ile]"      'narf::byte-compile)
(excmd! "acomp[ile]"      'narf::autoload-compile)
(excmd! "cd"              'narf::cd)
(excmd! "en[ew]"          'narf::create-file)
(excmd! "ren[ame]"        'narf::rename-this-file)          ; rename [NEWNAME]  # rename file
(excmd! "del[ete]"        'narf::delete-this-file)          ; delete[!]
(excmd! "al[ign]"         'narf::align)
(excmd! "retab"           'narf::retab)
(excmd! "na[rrow]"        'narf::narrow-indirect-or-widen)  ; Narrow buffer to selection
(excmd! "x"               'narf::scratch-buffer)
(excmd  "k[ill]"          'kill-this-buffer)                ; Kill current buffer
(excmd! "k[ill]o"         'narf:cleanup-buffers)            ; Kill current project buffers
(excmd! "k[ill]all"       'narf::kill-buffers)              ; Kill all buffers (bang = project buffers only)
(excmd! "k[ill]buried"    'narf::kill-buried-buffers)       ; Kill all buffers (bang = project buffers only)
(excmd! "ma[ke]"          'narf::build)
(excmd! "t"               'narf::tmux-run)
(excmd! "tcd"             'narf::tmux-chdir)
(excmd! "a"               'helm-projectile-find-other-file)
(excmd! "proj[ect]"       'helm-projectile-switch-project)
(excmd! "ag"              'narf::ag-search)
(excmd! "agr"             'narf::ag-regex-search)
(excmd! "ag[cw]d"         'narf::ag-search-cwd)
(excmd! "agr[cw]d"        'narf::ag-regex-search-cwd)
(excmd! "sw[oop]"         'narf::swoop)
(excmd! "rec[ent]"        'narf::recentf)
(excmd! "ref[actor]"      'emr-show-refactor-menu)
(excmd! "snip[pets]"      'narf::snippets)                  ; snip[!]
(excmd! "cap[ture]"       'helm-org-capture-templates)
(excmd! "n[otes]"         'helm-org-agenda-files-headings)
(after "flycheck"
  (excmd! "er[rors]"      (λ (flycheck-buffer) (flycheck-list-errors))))
(after "re-builder"
  (excmd "re[gex]"       'narf::regex))
(after "org"
  (excmd  "o[rg]edit"     'org-edit-special)
  (excmd  "o[rg]refile"   'org-refile)
  (excmd  "o[rg]archive"  'org-archive-subtree)
  (excmd  "o[rg]agenda"   'org-agenda)
  (excmd  "o[rg]todo"     'org-show-todo-tree)
  (excmd  "o[rg]link"     'org-link)
  (excmd  "o[rg]align"    'org-align-all-tags))
(after "workgroups2"
  (excmd! "sl[oad]"       'narf::load-session)
  (excmd! "ss[ave]"       'narf::save-session)
  (excmd  "wg"            (λ (message (wg-workgroup-list-display))))
  (excmd! "wnew"          'narf::new-workgroup)
  (excmd! "wre[name]"     'narf::rename-workgroup)
  (excmd  "wn[ext]"       'wg-switch-to-workgroup-right)
  (excmd  "wp[rev]"       'wg-switch-to-workgroup-left)
  (excmd  "wl[ast]"       'wg-switch-to-previous-workgroup)
  (excmd  "k[ill]w"       'wg-kill-workgroup)
  (excmd  "k[ill]ow"      (λ (let (workgroup (wg-current-workgroup))
                              (dolist (w (wg-workgroup-list))
                                (unless (wg-current-workgroup-p w)
                                  (wg-kill-workgroup w)))))))


(provide 'narf-commands)
;;; narf-commands.el ends here
