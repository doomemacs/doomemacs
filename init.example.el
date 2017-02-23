;;; init.el
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;; Version: 2.0.0
;;
;;   =================     ===============     ===============   ========  ========
;;   \\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
;;   ||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
;;   || . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
;;   ||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
;;   || . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
;;   ||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
;;   || . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
;;   ||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
;;   ||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
;;   ||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
;;   ||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
;;   ||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
;;   ||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
;;   ||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
;;   ||.=='    _-'                                                     `' |  /==.||
;;   =='    _-'                                                            \/   `==
;;   \   _-'                                                                `-_   /
;;    `''                                                                      ``'
;;
;; These demons are not part of GNU Emacs.
;;
;;; License: MIT

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature
       evil            ; come to the dark side, we have cookies
       ;; TODO jump            ; navigating your code
       snippets        ; my elves. They type so I don't have to
       file-templates  ; auto-snippets for empty files
       spellcheck      ; tasing you for misspelling mispelling
       syntax-checker  ; tasing you for every forgotten semicolon
       version-control ; remember, remember that commit in November
       workspaces      ; tab emulation, persistence and separate workspaces
       eval            ; repls, runners 'n builders; run code, run
       debug           ; stepping through code, to help you add bugs

       :completion
       company         ; auto-completion backend
       ;; TODO auto-complete   ; auto-completion backend #2
       ivy             ; a search engine for love and life
       ;; TODO helm            ;
       ;; ido             ;

       :ui
       doom            ; doom-one; a look inspired by Atom's Dark One
       doom-dashboard  ; a nifty splash screen for Emacs
       doom-modeline   ; a snazzy Atom-inspired mode-line
       doom-quit       ; DOOM quit-message prompts when you quit Emacs
       hl-todo         ; highlight TODO/FIXME/NOTE tags

       :tools
       electric-indent ; smarter, keyword-based electric-indent
       eshell          ; a consistent, cross-platform shell (WIP)
       ;; TODO dash            ; interacting with dash.app
       dired           ; making dired pretty [functional]
       restclient      ; A REST REPL
       rotate-text     ; cycle region at point between text candidates
       tmux            ; an API for interacting with tmux
       upload          ; map local to remote projects via ssh/ftp

       :lang
       assembly        ; assembly for fun or debugging
       cc              ; C/C++/Obj-C madness
       crystal         ; ruby at the speed of c
       csharp          ; unity, .NET, and mono shenanigans
       data            ; config/data formats
       emacs-lisp      ; drown in parentheses
       go              ; the hipster dialect
       haskell         ; a language that's lazier than I am
       java            ; the poster child for carpal tunnel syndrome
       javascript      ; all(hope(abandon(ye(who(enter(here))))))
       julia           ; a better, faster MATLAB
       latex           ; writing papers in Emacs has never been so fun
       lua             ; one-based indices? one-based indices
       markdown        ; writing docs for people to ignore
       octave          ; math isn't a choice, it's a way of life
       org             ; for organized fearless leader (WIP)
       php             ; make php less awful to work with
       processing      ; for prototyping
       python          ; beautiful is better than ugly
       ruby            ; 1.step do {|i| p "Ruby is #{i&1==0?'love':'life'}"}
       rust            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala           ; java, but good
       sh              ; she sells (ba|z)sh shells on the C xor
       swift           ; who asked for emoji variables?
       typescript      ; javascript, but better
       web             ; the tubes

       ;; Applications are large, toggle-able states that transform Emacs to
       ;; fulfill a specific purpose. See `doom/toggle'. They should be loaded
       ;; last.
       :app
       ;; TODO crm             ; org-mode for client relations management
       ;; TODO db              ; Emacs as a database browser
       ;; TODO email           ; Emacs as an email client
       ;; TODO finance         ; keeping track of my shekels
       ;; TODO present         ; showing off presentations in emacs
       ;; TODO rss             ; emacs as an RSS reader
       ;; TODO stream          ; for streaming code (https://livecoding.tv/vnought)
       ;; TODO twitter         ; twitter client https://twitter.com/vnought
       ;; TODO write           ; emacs as a word processor (latex + org + markdown)

       ;; Private modules are aren't tracked in the repo, except for mine. Use
       ;; it as a reference for your own.
       :private hlissner)

