;;; init.el
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;; Version: 0.9.9
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
(defconst narf-default-font (font-spec :family "Hack" :size 12))

(setq user-full-name "Henrik Lissner"
      user-mail-address "henrik@lissner.net")

(narf `(core ; core/core.el

        ;; OS-specific config
        ,(cond (IS-MAC      'core-os-osx)
               (IS-LINUX    'core-os-linux)
               (IS-WINDOWS  'core-os-win32))

        ;; The heart of NARF
        core-popup          ; taming sudden and inevitable windows
        core-ui             ; draw me like one of your French editors
        core-evil           ; come to the dark side, we have cookies
        core-editor         ; filling the editor-shaped hole in the emacs OS
        core-company        ; for the lazy typist
        core-yasnippet      ; for the lazier typist
        core-file-templates ; for the laziest typist
        core-flycheck       ; get tazed for every semicolon you forget
        core-project        ; whose project am I in?
        core-vcs            ; remember remember, that commit in November
        core-helm           ; a search engine for life and love
        core-sessions       ; cure Emacs alzheimers + tab emulation
        core-eval           ; run code, run; debugging too

        ;; Environments
        module-apple        ; Applescript, Swift, Launchbar, iOS, wallet syphons, etc.
        module-cc           ; C/C++/Obj-C madness
        module-crystal      ; ruby at the speed of c
        module-csharp       ; unity, .NET, and mono shenanigans
        module-data         ; config and data formats
        module-go           ; the hipster dialect
        module-haskell      ; a language that's lazier than I am
        module-java         ; the poster child for carpal tunnel syndome
        module-js           ; all(hope(abandon(ye(who(enter(here))))))
        module-julia        ; MATLAB, but fast
        module-latex        ; for writing papers in Emacs
        module-lisp         ; drowning in parentheses
        module-lua          ; one-based indices? one-based indices.
        module-org          ; for organized fearless leader
        module-php          ; making php less painful to work with
        module-processing   ; pretty prototypes
        module-python       ; beautiful is better than ugly
        module-ruby         ; 1.step do {|i| p "Ruby is #{i&1==0?'love':'life'}"}
        module-rust         ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        module-scala        ; Java, but good
        module-sh           ; she sells Z-shells by the C XOR
        module-text         ; writing docs for people to ignore + latex
        module-web          ; #big-bang::before { content: ""; }

        ;; Experimental
        ;;module-eshell       ; for inferior OSes *cough*windows

        ;; Extra libraries
        extra-demo          ; allow me to demonstrate...
        extra-tags          ; if you liked it you should've generated a tag for it
        extra-tmux          ; close the rift between GUI & terminal
        extra-write         ; for writing fiction in Emacs

        ;; Customization
        my-bindings
        my-commands
        ))

;;; I think so Brain...
