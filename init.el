;;; init.el --- NARF bootstrap
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/emacs.d
;; Version: 0.8.0
;;
;;; Are you pondering what I'm pondering, Pinky?
;;
;;                           ,,,        !/:.
;;                          /::\".      !!:::
;;                          :::::\".  ," \:,::
;;                          ::::::\ ". ,","\::.
;;                          \:::::":\ "/""v' :'
;;                           !::::\   !    \ \   __
;;                            "::::\  \     ! \.&&&&,
;;                               ," __ ",  cd,&&&&&&'
;;                               \    ". "" / \&&&"                       _,---
;;                                 "",__\_        /                    _,:":::::
;;                               _," ,""  ,-,__,/":,_                ,",":::::::
;;                            _,"  ,"     `''   ::::,",__,,----,,__," /:::::::::
;;                         ,"   ,".__,          \:::,"            "  /:::":::::/
;;                       ,"  ,/"::::::\          >"                 (_-"/::::::
;;                      /  ,"_!:::::::/,       ,"              _,,--,  /::::::/
;;                    /   "" _,"\:::::::'     !              ,"      ){:::::/
;;                   !    _,"    \ "",         \,"""-,____,"__,,,"_," _/
;;                    ""t"       \\   \          "-,_(*)&&&&(*)," \ ."
;;                     /          \",  !            ,   \   ! -    )
;;                     !          \  ""             !    !==!"-,__,'
;;                     !           \                 """_""""`, ", /"_
;;                     \       ,   .l                 /" "     ", \! ,_/
;;                      ),     \   / \                \/       ,, /! !
;;                    ,::\      \,"   \                !        \/ ! !
;;                _,::::" )     )\  ,"  ___            \ -,_,  ,"",! !
;;         __,,,::::""   ,"   ,":::,-:::--:"           __\_!__/_""-,_!
;;   ,,:::"""""""      ,:_,""__...._"""::::""       /:::::" ""::::::
;;  (:._              l::::::::::::\\/               ""          ""
;;    """"--,,,---              """"
;;
;; These mice are not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst EMACS-WRITE nil)

(defconst narf-theme        'narf-dark)
(defconst narf-default-font (font-spec :family "Terminus (TTF)" :size 12 :antialias nil))
(defconst narf-writing-font (font-spec :family "Hack" :size 14))
(defconst narf-big-font     (font-spec :family "Inconsolata" :size 20))

(defconst narf-packages
  '(core ; core/core.el

    ;;; The heart of NARF
    core-popup           ; taming stray windows
    core-ui              ; draw me like one of your French editors
    core-evil            ; come to the dark side, we have cookies
    core-editor          ; filling the editor-shaped hole in the emacs OS
    core-company         ; for the lazy typist
    core-yasnippet       ; for the lazier typist
    core-auto-insert     ; for the laziest typist
    core-flycheck        ; remember that semicolon you forgot?
    core-project         ; whose project am I in?
    core-vcs             ; version control is a programmer's best friend
    core-helm            ; a search engine for life and love
    core-eval            ; run code, run.
    core-workgroups      ; cure Emacs alzheimers + tab emulation

    ;;; Environments
    module-cc            ; c/c++/obj-c madness
    module-csharp        ; unity, .NET, and mono shenanigans
    module-data          ; dbs 'n data formats
    module-go            ; the hipster dialect
    module-java          ; the poster child for carpal tunnel syndome
    module-js            ; all(hope(abandon(ye(who(enter(here))))))
    module-lisp          ; drowning in parentheses
    module-lua           ; one-based indices? one-based indices.
    module-markdown      ; markdown
    ;; module-org        ; for fearless [organized] leader
    module-php           ; making php less painful to work with
    module-plantuml      ; to help show others how right I am
    module-python        ; beautiful is better than ugly
    module-regex         ; /^[^\s](meaning)[^\n]*/
    module-ruby          ; <3
    module-rust          ; Fe2O3; for gamedev and great good
    module-sh            ; she sells Z-shells by the C XOR
    module-swift         ; yay, emoji variables!
    module-vim           ; my mistress
    module-web           ; for the 2.0'er

    ;;; Experimental
    ;; module-crystal    ; ruby at the speed of c
    ;; module-eshell     ; for inferior OSes *cough*windows
    ;; module-nim        ; look out Abraham
    ;; module-pony       ; Dear Princess Compiler

    ;;; Specific custom functionality
    lib-plugin           ; plugin dev for various programs
    lib-tmux             ; closing the rift between GUI & terminal
    lib-demo             ; let me demonstrate...
    lib-writing          ; yes, I write papers and fiction in emacs
    ;; lib-crm           ; emacs and org-mode based CRM

    ;;; Key bindings & ex commands
    my-bindings
    my-commands
    ))

(load (concat user-emacs-directory "init-packages.el"))

;;; I think so Brain...
