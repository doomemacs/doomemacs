;;; init.el
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;; Version: 1.3.1
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

(load (concat user-emacs-directory "core/core") nil t)

(doom! :emacs
       dired
       eshell
       org         ; for organized fearless leader

       :apps
       crm
       email
       finance
       present
       rss
       stream      ;
       twitter
       write

       :lang
       asm         ; assembly for fun
       cc          ; C/C++/Obj-C madness
       crystal     ; ruby at the speed of c
       csharp      ; unity, .NET, and mono shenanigans
       css         ; #big-bang::before { content: ""; }
       data        ; config/data formats
       emacs-lisp  ; drown in parentheses
       go          ; the hipster dialect
       haskell     ; a language that's lazier than I am
       java        ; the poster child for carpal tunnel syndrome
       javascript  ; all(hope(abandon(ye(who(enter(here))))))
       julia       ; a better, faster MATLAB
       latex       ; write papers in Emacs
       lua         ; one-based indices? one-based indices
       octave      ; math isn't a choice, it's a way of life
       php         ; make php less awful to work with
       processing  ; for prototyping
       python      ; beautiful is better than ugly
       rest        ; emacs as a service
       ruby        ; 1.step do {|i| p "Ruby is #{i&1==0?'love':'life'}"}
       rust        ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala       ; java, but good
       sh          ; she sells (ba|z)sh shells on the C xor
       swift       ; who asked for emoji variables?
       text        ; writing docs for people to ignore
       typescript  ; javascript, but better
       web         ; The end is always near </html>
       vim
       )
