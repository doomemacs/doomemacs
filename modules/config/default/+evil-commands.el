;;; config/default/+evil-commands.el -*- lexical-binding: t; -*-

(defalias 'ex! 'evil-ex-define-cmd)

(evil-define-command doom:cleanup-session (bang)
  (interactive "<!>")
  (doom/cleanup-session bang))

(evil-define-operator doom:open-scratch-buffer (bang)
  (interactive "<!>")
  (doom/open-scratch-buffer bang))

(evil-define-command doom:pwd (bang)
  (interactive "<!>")
  (if (not bang)
      (pwd)
    (kill-new default-directory)
    (message "Copied to clipboard")))

(evil-define-command doom:make (command &optional from-pwd)
  (interactive "<sh><!>")
  (let ((default-directory (if from-pwd default-directory (doom-project-root t)))
        (command (and command (evil-ex-replace-special-filenames command))))
    (compile command)))


;;
;; Commands
;;

;;; Commands defined elsewhere
;;(ex! "al[ign]"      #'+evil:align)
;;(ex! "g[lobal]"     #'+evil:global)

;;; Custom commands
;; Editing
(ex! "@"            #'+evil:macro-on-all-lines)   ; TODO Test me
(ex! "al[ign]"      #'+evil:align)
(ex! "ral[ign]"     #'+evil:align-right)
(ex! "enhtml"       #'+web:encode-html-entities)
(ex! "dehtml"       #'+web:decode-html-entities)
(ex! "mc"           #'+evil:mc)
(ex! "iedit"        #'evil-multiedit-ex-match)
(ex! "na[rrow]"     #'+evil:narrow-buffer)
(ex! "retab"        #'+evil:retab)
;; External resources
;; TODO (ex! "db"          #'doom:db)
;; TODO (ex! "dbu[se]"     #'doom:db-select)
;; TODO (ex! "go[ogle]"    #'doom:google-search)
(ex! "lo[okup]"    #'+lookup:online)
(ex! "dash"        #'+lookup:dash)
(ex! "dd"          #'+lookup:devdocs)
(ex! "http"        #'httpd-start)            ; start http server
(ex! "repl"        #'+eval:repl)             ; invoke or send to repl
;; TODO (ex! "rx"          'doom:regex)             ; open re-builder
(ex! "sh[ell]"     #'+eshell:run)
(ex! "t[mux]"      #'+tmux:run)              ; send to tmux
(ex! "tcd"         #'+tmux:cd-here)          ; cd to default-directory in tmux
(ex! "x"           #'doom:open-scratch-buffer)
;; GIT
(ex! "gist"        #'+gist:send)  ; send current buffer/region to gist
(ex! "gistl"       #'+gist:list)  ; list gists by user
(ex! "gbrowse"     #'+vcs/git-browse)        ; show file in github/gitlab
(ex! "gissues"     #'+vcs/git-browse-issues) ; show github issues
(ex! "git"         #'magit-status)           ; open magit status window
(ex! "gstage"      #'magit-stage)
(ex! "gunstage"    #'magit-unstage)
(ex! "gblame"      #'magit-blame)
(ex! "grevert"     #'git-gutter:revert-hunk)
;; Dealing with buffers
(ex! "clean[up]"   #'doom:cleanup-session)
(ex! "k[ill]"      #'doom/kill-this-buffer)
(ex! "k[ill]all"   #'+default:kill-all-buffers)
(ex! "k[ill]m"     #'+default:kill-matching-buffers)
(ex! "k[ill]o"     #'doom/kill-other-buffers)
(ex! "l[ast]"      #'doom/popup-restore)
(ex! "m[sg]"       #'view-echo-area-messages)
(ex! "pop[up]"     #'doom/popup-this-buffer)
;; Project navigation
(ex! "a"           #'projectile-find-other-file)
(ex! "cd"          #'+default:cd)
(ex! "pwd"         #'doom:pwd)
(cond ((featurep! :completion ivy)
       (ex! "ag"       #'+ivy:ag)
       (ex! "agc[wd]"  #'+ivy:ag-from-cwd)
       (ex! "rg"       #'+ivy:rg)
       (ex! "rgc[wd]"  #'+ivy:rg-from-cwd)
       (ex! "pt"       #'+ivy:pt)
       (ex! "ptc[wd]"  #'+ivy:pt-from-cwd)
       (ex! "grep"      #'+ivy:grep)
       (ex! "grepc[wd]" #'+ivy:grep-from-cwd)
       (ex! "sw[iper]" #'+ivy:swiper)
       (ex! "todo"     #'+ivy:todo))
      ((featurep! :completion helm)
       (ex! "ag"       #'+helm:ag)
       (ex! "agc[wd]"  #'+helm:ag-cwd)
       (ex! "rg"       #'+helm:rg)
       (ex! "rgc[wd]"  #'+helm:rg-cwd)
       (ex! "sw[oop]"  #'+helm:swoop)
       (ex! "todo"     #'+helm:todo)))
;; Project tools
(ex! "mak[e]"      #'doom:make)
(ex! "debug"       #'+debug/run)
(ex! "er[rors]"    #'flycheck-list-errors)
;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)
;; Sessions/tabs
(ex! "sclear"      #'+workspace/kill-session)
(ex! "sl[oad]"     #'+workspace:load-session)
(ex! "ss[ave]"     #'+workspace:save-session)
(ex! "tabc[lose]"  #'+workspace:delete)
(ex! "tabclear"    #'doom/kill-all-buffers)
(ex! "tabl[ast]"   #'+workspace/switch-to-last)
(ex! "tabload"     #'+workspace:load)
(ex! "tabn[ew]"    #'+workspace:new)
(ex! "tabn[ext]"   #'+workspace:switch-next)
(ex! "tabp[rev]"   #'+workspace:switch-previous)
(ex! "tabr[ename]" #'+workspace:rename)
(ex! "tabs"        #'+workspace/display)
(ex! "tabsave"     #'+workspace:save)
;; Org-mode
(ex! "cap"         #'+org-capture/dwim)

