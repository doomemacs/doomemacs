;;; init.el
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
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
       jump            ; helping you navigate your code base
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
       ;; helm            ;
       ;; ido             ;

       :ui
       doom            ; doom-one; a look inspired by Atom's Dark One
       doom-dashboard  ; a nifty splash screen for Emacs
       doom-modeline   ; a snazzy Atom-inspired mode-line
       doom-quit       ; DOOM quit-message prompts when you quit Emacs
       hl-todo         ; highlight TODO/FIXME/NOTE tags

       :tools
       dired           ; making dired pretty [functional]
       electric-indent ; smarter, keyword-based electric-indent
       eshell          ; a consistent, cross-platform shell (WIP)
       gist            ; manage & create gists
       macos           ; macos-specific commands
       rotate-text     ; cycle region at point between text candidates
       term            ; Emacs as a terminal emulator
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
       org             ; for organized fearless leader (WIP)
       php             ; make php less awful to work with
       python          ; beautiful is better than ugly
       rest            ; Emacs as a REST client
       ruby            ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala           ; java, but good
       sh              ; she sells (ba|z)sh shells on the C xor
       swift           ; who asked for emoji variables?
       typescript      ; javascript, but better
       web             ; the tubes

       ;; Applications are opinionated modules that transform Emacs to fulfill a
       ;; specific purpose. They should be loaded last.
       :app
       ;; TODO crm        ; org-mode for client relations management
       email           ; Emacs as an email client
       ;; TODO finance    ; keeping track of my shekels
       ;; TODO irc        ; how neckbeards socialize
       present         ; showing off presentations in emacs
       rss             ; emacs as an RSS reader
       twitter         ; twitter client https://twitter.com/vnought
       write           ; emacs as a word processor (latex + org + markdown)

       ;; Private modules are aren't tracked in the repo, except for mine. Use
       ;; it as a reference for your own.
       :private hlissner)

