;;; init.el --- NARF bootstrap
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/emacs.d
;; Version: 0.4.0
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

(defconst narf-theme        'narf-dark)
(defconst narf-default-font (font-spec :family "DejaVu Sans Mono" :size 12))
(defconst narf-big-font     (font-spec :family "Ubuntu Mono"      :size 18))

(scroll-bar-mode -1)  ; no scrollbar
(tool-bar-mode   -1)  ; no toolbar

;; prematurely optimize for faster startup
(let (file-name-handler-alist)
  (load (concat user-emacs-directory "init-load-path.el"))
  (load-theme narf-theme t)

  (mapc 'require
        `(core ; core/core.el
          ,(cond (IS-MAC      'core-os-osx)
                 (IS-LINUX    'core-os-linux)
                 (IS-WINDOWS  'core-os-win32))

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
          core-quickrun        ; run code, run.
          core-workgroups      ; cure Emacs alzheimers

          module-cc            ; c/c++/obj-c madness
          ;; module-crystal    ; ruby at the speed of c
          module-csharp        ; unity, .NET, and mono shenanigans
          module-collab        ; wonewy, I'm so wonewy~
          module-data          ; dbs 'n data formats
          module-elisp         ; drowning in parentheses
          module-eshell        ; eshell (on windows)
          module-go            ; a hipster dialect
          module-java          ; the poster child for carpal tunnel syndome
          module-js            ; alert("not java, javascript!")
          module-lb6           ; LaunchBar 6 development
          module-lua           ; one-based indices? one-based indices.
          module-markdown      ; markdown
          module-org           ; for fearless [organized] leader
          module-plantuml      ; to help show how right I am
          module-php           ; making php less painful to work with
          module-python        ; beautiful is better than ugly
          module-regex         ; /^[^\s](meaning)[^\n]*/
          module-ruby          ; <3
          module-rust          ; Fe2O3
          module-sh            ; she sells Z-shells by the C XOR
          module-swift         ; yay, emoji variables!
          module-vim           ; my mistress
          module-web           ; for the 2.0'er

          my-bindings
          my-commands
          ))
  (narf-init))

;;; I think so Brain...
