(defalias 'ex! 'evil-ex-define-cmd)

;;; Custom commands
;; Emacs utilities
(ex! "bc[omp]"      '+hlissner:byte-compile)
(ex! "clog"         'global-command-log-mode)
(ex! "minor"        'describe-minor-mode) ; list minor modes
(ex! "re[load]"     'doom/reload)
(ex! "re[load]au"   'doom/reload-autoloads)

;; Editing
(ex! "@"            '+evil:macro-on-all-lines)   ; TODO Test me
(ex! "date"         '+text:insert-date)
(ex! "enhtml"       '+hlissner:encode-html-entities)
(ex! "dehtml"       '+hlissner:decode-html-entities)
(ex! "ie[dit]"      'evil-multiedit-ex-match)
(ex! "na[rrow]"     '+hlissner:narrow-buffer)
(ex! "ref[actor]"   'emr-show-refactor-menu)
(ex! "retab"        '+hlissner:retab)

;; External resources
;; TODO (ex! "db"          'doom:db)
;; TODO (ex! "dbu[se]"     'doom:db-select)
;; TODO (ex! "go[ogle]"    'doom:google-search)
(ex! "http"        'httpd-start)            ; start http server
(ex! "repl"        '+eval/repl)             ; invoke or send to repl
;; TODO (ex! "rx"          'doom:regex)             ; open re-builder
(ex! "sh[ell]"     '+eshell:run)
(ex! "t[mux]"      '+tmux:run)              ; send to tmux
(ex! "tcd"         '+tmux:cd-here)          ; cd to default-directory in tmux
(ex! "x"           'doom:scratch-buffer)

;; GIT
(ex! "gbrowse"     '+vcs/git-browse)        ; show file in github/gitlab
(ex! "gissues"     '+vcs/git-browse-issues) ; show github issues
(ex! "git"         'magit-status)           ; open magit status window
(ex! "gstage"      'magit-stage)
(ex! "gunstage"    'magit-unstage)
;; TODO :gblame
;; TODO :grevert
;; TODO :gblame

;; Dealing with buffers
(ex! "clean[up]"   'doom/cleanup-buffers)
(ex! "k[ill]"      'doom/kill-this-buffer)
(ex! "k[ill]all"   '+hlissner:kill-all-buffers)
(ex! "k[ill]m"     '+hlissner:kill-matching-buffers)
(ex! "k[ill]o"     'doom/kill-other-buffers)
(ex! "l[ast]"      'doom/popup-restore)
(ex! "m[sg]"       'view-echo-area-messages)
(ex! "pop[up]"     'doom/popup) ; open current buffer in popup

;; Project navigation
(ex! "a"           'projectile-find-other-file)
(ex! "ag"          '+ivy:ag-search)
(ex! "ag[cw]d"     '+ivy:ag-search-cwd)
(ex! "cd"          '+hlissner:cd)
(ex! "sw[iper]"    '+ivy:swiper)     ; in-file search

;; Project tools
(ex! "build"       '+eval/build)
(ex! "debug"       '+debug/run)
(ex! "er[rors]"    'flycheck-list-errors)
(ex! "todo"        '+ivy/tasks)

;; File operations
(ex! "mv"          '+evil:file-move)
(ex! "rm"          '+evil:file-delete)

;; Sessions/tabs
(ex! "sclear"      '+workspace/kill-session)
(ex! "sl[oad]"     '+workspace:load-session)
(ex! "ss[ave]"     '+workspace:save-session)
(ex! "tabc[lose]"  '+workspace:delete)
(ex! "tabclear"    'doom/kill-all-buffers)
(ex! "tabl[ast]"   '+workspace/switch-to-list)
(ex! "tabload"     '+workspace:load)
(ex! "tabn[ew]"    '+workspace:new)
(ex! "tabn[ext]"   '+workspace:switch-next)
(ex! "tabp[rev]"   '+workspace:switch-previous)
(ex! "tabr[ename]" '+workspace:rename)
(ex! "tabs"        '+workspace/display)
(ex! "tabsave"     '+workspace:save)

;; Org-mode
(ex! "org"         '+org:capture)
