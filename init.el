;;; NARF! Emacs for the jaded vimmer
;;
;; Author: Henrik Lissner <henrik@lissner>
;; URL: https://github.com/hlissner/emacs.d
;;
;;; Narf!
;;
;;                           ,,,        !/:.
;;                          /::\".      !!:::
;;                          :::::\".  ," \:,::
;;                          ::::::\ ". ,","\::.
;;                          \:::::":\ "/""V' :'
;;                           !::::\   !    \ \   __
;;                            "::::\  \     ! \.&&&&,
;;                               ," __ ",  CD,&&&&&&'
;;                               \    ". "" / \&&&"                       _,---
;;                                 "",__\_        /                    _,:":::::
;;                               _," ,""  ,-,__,/":,_                ,",":::::::
;;                            _,"  ,"     `''   ::::,",__,,----,,__," /:::::::::
;;                         ,"   ,".__,          \:::,"            "  /:::":::::/
;;                       ,"  ,/"::::::\          >"                 (_-"/::::::
;;                      /  ,"_!:::::::/,       ,"              _,,--,  /::::::/
;;                    /   "" _,"\:::::::'     !              ,"      ){:::::/
;;                   !    _,"    \ "",         \,"""-,____,"__,,,"_," _/
;;                    ""T"       \\   \          "-,_(*)&&&&(*)," \ ."
;;                     /          \",  !            ,   \   ! -    )
;;                     !          \  ""             !    !==!"-,__,'
;;                     !           \                 """_""""`, ", /"_
;;                     \       ,   .L                 /" "     ", \! ,_/
;;                      ),     \   / \                \/       ,, /! !
;;                    ,::\      \,"   \                !        \/ ! !
;;                _,::::" )     )\  ,"  ___            \ -,_,  ,"",! !
;;         __,,,::::""   ,"   ,":::,-:::--:"           __\_!__/_""-,_!
;;   ,,:::"""""""      ,:_,""__...._"""::::""       /:::::" ""::::::
;;  (:._              L::::::::::::\\/               ""          ""
;;    """"--,,,---              """"
;;
;;; Code:
(defconst DEBUG-MODE nil)

(defconst DEFAULT-FONT  (font-spec :family "Terminus (TTF)" :size 12 :antialias nil))
(defconst DEFAULT-THEME 'narf-dark) ; for GUI client
(defconst TERM-THEME    'wombat)    ; for <256 color terminals

(load (concat user-emacs-directory "core/startup.el"))
(narf/init
 '(core                ; yoink @ core.el
   core-ui             ; aesthetics
   core-evil           ; evil-mode and its plugins
   core-editor         ; completing the editor
   core-company        ; for the lazy typist

   init-auto-insert    ; see above
   init-fly            ; fly(check|spell)
   init-vc             ; version control gutter + git modes
   init-ido            ; a search engine for your car keys
   init-helm           ; a search engine for your life
   init-project        ; dired, neotree

   init-cc             ; C/C++/Obj-C madness
   ;; init-cscope
   ;; init-csharp      ; unity, mono and xamarin
   init-data           ; DBs 'n data formats
   ;; init-eshell
   ;; init-go
   init-java           ; the poster child for carpal tunnel syndome
   init-js             ; alert("not java, javascript!")
   init-lisp           ; elisp, clisp and clojure
   init-lua            ; one-based indices? One-based indices.
   init-org            ; for fearless [organized] leader
   init-php            ; making php less painful to work with
   init-python         ; beautiful is better than ugly
   init-regex          ; /^[^\s](meaning)[^\n]*/
   init-ruby           ; <3
   init-scss           ; @include magic;
   init-sh             ; #!/bin/bash_your_head_in

   ;; init-sonicpi          ;
   ;; init-swift       ; yay, emoji variabless!
   init-text           ; I got nothing...
   ;; init-rust
   ;; init-r           ; for science!
   init-vim            ; the confessional
   init-web            ; for the 2.0'er
   init-workgroups     ; session management I can understand
   init-yasnippet      ; type for me

   narf-bindings
   narf-commands
   narf-settings
   ))


;;; Are you pondering what I'm pondering Pinky?
