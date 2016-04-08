;;; init.el
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;; Version: 0.9.8
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
;;; License: MIT

(load (concat user-emacs-directory "bootstrap.el"))
;;

(defconst narf-default-theme 'narf-dark)
(defconst narf-default-font (font-spec :family "Terminus (TTF)" :size 12 :antialias nil))

(narf '(core ; core/core.el

        ;; The heart of NARF
        core-popup           ; taming sudden and inevitable windows
        core-ui              ; draw me like one of your French editors
        core-evil            ; come to the dark side, we have cookies
        core-editor          ; filling the editor-shaped hole in the emacs OS
        core-completion      ; for the lazy typist
        core-yasnippet       ; for the lazier typist
        core-file-templates  ; for the laziest typist
        core-flycheck        ; code police; tazing you for every semicolon you forget
        core-project         ; whose project am I in?
        core-vcs             ; remember remember, that commit in November
        core-helm            ; a search engine for life and love
        core-eval            ; run code, run.
        core-debug           ; emacs as a universal debugger
        core-sessions        ; cure Emacs alzheimers + tab emulation

        ;; Environments
        module-apple         ; Applescript, Swift, Launchbar, iOS, wallet syphons, etc.
        module-cc            ; c/c++/obj-c madness
        module-crystal       ; ruby at the speed of c
        module-csharp        ; unity, .NET, and mono shenanigans
        module-go            ; the hipster dialect
        module-haskell       ; a language that's lazier than I am
        module-java          ; the poster child for carpal tunnel syndome
        module-js            ; all(hope(abandon(ye(who(enter(here))))))
        module-lisp          ; drowning in parentheses
        module-lua           ; one-based indices? one-based indices.
        module-php           ; making php less painful to work with
        module-python        ; beautiful is better than ugly
        module-ruby          ; 1.step do {|i| p "Ruby is #{i&1==0?'love':'life'}"}
        module-rust          ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        module-sh            ; she sells Z-shells by the C XOR
        module-text          ; writing docs for people to ignore
        module-web           ; for the 2.0'er

        ;; Experimental
        ;;module-eshell        ; for inferior OSes *cough*windows

        ;; Organizational/Notes
        ;;module-org           ; for organized fearless leader
        ;;module-org-crm       ; to keep tabs on my victims
        ;;module-org-notebook  ; #modernizeOrgMode2016

        ;; Extra Tools
        module-tmux          ; closing the rift between GUI & terminal
        module-demo          ; allow me to demonstrate...
        module-ansible       ;
        ;;module-write       ; for writing papers and fiction in Emacs

        ;; Key bindings & ex commands
        my-bindings
        my-commands
        ))

;;; I think so Brain...
