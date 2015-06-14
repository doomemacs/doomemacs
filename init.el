;;; init.el --- NARF bootstrap
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/emacs.d
;; Version: 0.1.0
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

(defconst narf-debug-mode nil)

(defconst narf-default-theme  (if window-system 'narf-dark 'wombat))
(defconst narf-default-font  '(:family "terminus (ttf)" :size 12 :antialias nil))
(defconst narf-big-font      '(:family "Inconsolata" :size 18 :antialias t))

(load (concat user-emacs-directory "init-load-path.el"))
(mapc 'require
      '())
      ;; (;; Settings for specific modes or tools
       ;;  init-cc             ; c/c++/obj-c madness
       ;;  ;; init-cscope
       ;;  ;; init-csharp      ; unity, mono and xamarin
       ;;  init-data           ; dbs 'n data formats
       ;;  ;; init-eshell
       ;;  ;; init-go
       ;;  init-java           ; the poster child for carpal tunnel syndome
       ;;  init-js             ; alert("not java, javascript!")
       ;;  init-lisp           ; elisp, clisp and clojure
       ;;  init-lua            ; one-based indices? one-based indices.
       ;;  init-org            ; for fearless [organized] leader
       ;;  init-php            ; making php less painful to work with
       ;;  init-python         ; beautiful is better than ugly
       ;;  init-regex          ; /^[^\s](meaning)[^\n]*/
       ;;  init-ruby           ; <3
       ;;  init-scss           ; @include magic;
       ;;  init-sh             ; #!/bin/bash_your_head_in
       ;;  ;; init-sonicpi     ; the funk soul brotha
       ;;  ;; init-swift       ; yay, emoji variabless!
       ;;  init-text           ; i got nothing...
       ;;  ;; init-rust
       ;;  ;; init-r           ; for science!
       ;;  init-vim            ; the confessional
       ;;  init-web            ; for the 2.0'er

       ;;  bindings
       ;;  commands
       ;;  )
;; )

(require 'local nil t)
(message ">>> Loaded in %s" (emacs-init-time))

(defun display-startup-echo-area-message ()
  (message ">>> Loaded in %s" (emacs-init-time)))

;;; I think so Brain...
