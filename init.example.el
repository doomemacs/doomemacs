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

(setq user-emacs-directory "~/work/conf/doom-emacs-old")

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature
       evil            ; come to the dark side, we have cookies
       completion      ; company-mode & auto-complete; for lazy typists
       dev             ; tools for tweaking DOOM Emacs
       ivy             ; a search engine for love and life
       snippets        ; my elves. They type so I don't have to
       syntax-checking ; tasing you for every missing semicolon
       spell-checking  ; tasing you for mispellings and grammar derps
       version-control ; remember, remember that commit in November
       file-templates  ; auto-snippets for empty files, for lazier typists
       repl            ; run code, run
       jump            ; navigating your code
       workspaces      ; tab emulation, persistence and separate workspaces

       :ui
       doom            ; doom-one; a look inspired by Atom's Dark One
       doom-dashboard  ; a nifty splash screen for Emacs
       doom-modeline   ; a snazzy Atom-inspired mode-line
       git-gutter      ; diffs in the fringe

       :fun
       doom-quit       ; DOOM quit-message prompts when you quit Emacs

       :emacs
       autoinsert      ; file templates, for the laziest typists
       dired           ;
       eshell          ; a consistent, cross-platform shell
       ido             ;
       org             ; for organized fearless leader

       :tool
       dash            ; dash.app
       google          ;
       tmux            ;
       rest            ;

       ;; Applications are large, toggle-able states that transform Emacs to
       ;; fulfill a specific purpose. See `doom/toggle'.
       :app
       crm             ; org-mode for client relations management
       db              ; Emacs as a database browser
       email           ; Emacs as an email client
       finance         ; keeping track of my shekels
       present         ; showing off presentations in emacs
       rss             ; emacs as an RSS reader
       stream          ; for streaming code (https://livecoding.tv/vnought)
       twitter         ; twitter client https://twitter.com/vnought
       write           ; emacs as a word processor (latex + org + markdown)

       :lang
       asm             ; assembly for fun or debugging
       cc              ; C/C++/Obj-C madness
       crystal         ; ruby at the speed of c
       csharp          ; unity, .NET, and mono shenanigans
       css             ; #big-bang::before { content: ""; }
       data            ; config/data formats
       emacs-lisp      ; drown in parentheses
       git             ; various git files
       go              ; the hipster dialect
       haskell         ; a language that's lazier than I am
       html            ; The end is always near </html>
       java            ; the poster child for carpal tunnel syndrome
       javascript      ; all(hope(abandon(ye(who(enter(here))))))
       julia           ; a better, faster MATLAB
       latex           ; writing papers in Emacs has never been so fun
       lua             ; one-based indices? one-based indices
       octave          ; math isn't a choice, it's a way of life
       php             ; make php less awful to work with
       processing      ; for prototyping
       python          ; beautiful is better than ugly
       rest            ; emacs as a REST client
       ruby            ; 1.step do {|i| p "Ruby is #{i&1==0?'love':'life'}"}
       rust            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala           ; java, but good
       sh              ; she sells (ba|z)sh shells on the C xor
       swift           ; who asked for emoji variables?
       text            ; writing docs for people to ignore
       typescript      ; javascript, but better
       viml            ; PURGE THE HERETICS, FOR THE EMPEROR

       :frameworks
       angular
       jekyll
       laravel
       nodejs
       rails
       react
       launchbar
       screeps

       ;; Private modules are aren't tracked in the repo. Only one module is
       ;; included with DOOM emacs here. Mine! Feel free to copy it to your own
       ;; module and have fun.
       :private hlissner)

