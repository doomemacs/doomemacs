;;; init.el
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;; Version: 1.1.0
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
;;; License: GPLv3

(load (concat user-emacs-directory "bootstrap.el"))
;;

(defconst doom-default-theme  'doom-one)
(defconst doom-terminal-theme 'doom-dark)
(defconst doom-default-font (font-spec :family "Fira Mono" :size 12))

(defconst doom-leader ","       "Prefix for <leader> bindings")
(defconst doom-localleader "\\" "Prefix for <localleader> bindings")

(doom `(core ; core/core.el

        ,(cond (IS-MAC      'core-os-osx)
               (IS-LINUX    'core-os-linux)
               (IS-WINDOWS  'core-os-win32))

        ;; The heart of DOOM
        core-ui            ; draw me like one of your French editors
        core-evil          ; come to the dark side, we have cookies
        core-editor        ; filling the editor-shaped hole in the emacs OS
        core-company       ; for the lazy typist
        core-yasnippet     ; for the lazier typist
        core-autoinsert    ; for the laziest typist
        core-flycheck      ; get tazed for every semicolon you forget
        core-project       ; whose project am I in?
        core-vcs           ; remember remember, that commit in November
        core-helm          ; a search engine for life and love
        core-workgroups    ; cure Emacs alzheimers + tab emulation
        core-eval          ; run code, run; debug too
        core-popup         ; taming sudden and inevitable windows

        ;; Environments
        module-apple       ; Applescript, Swift, Launchbar & other wallet syphons
        module-cc          ; C/C++/Obj-C madness
        module-crystal     ; ruby at the speed of c
        module-csharp      ; unity, .NET, and mono shenanigans
        module-css         ; #big-bang::before { content: ""; }
        module-data        ; config and data formats
        module-go          ; the hipster dialect
        module-haskell     ; a language that's lazier than I am
        module-java        ; the poster child for carpal tunnel syndrome
        module-js          ; all(hope(abandon(ye(who(enter(here))))))
        module-julia       ; MATLAB, but fast
        module-latex       ; for writing papers in Emacs
        module-lisp        ; drowning in parentheses
        module-lua         ; one-based indices? one-based indices.
        module-org         ; for organized fearless leader
        module-php         ; making php less painful to work with
        module-processing  ; pretty prototypes
        module-python      ; beautiful is better than ugly
        module-ruby        ; 1.step do {|i| p "Ruby is #{i&1==0?'love':'life'}"}
        module-rust        ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        module-scala       ; Java, but good
        module-sh          ; she sells Z-shells by the C XOR
        module-text        ; writing docs for people to ignore
        module-web         ; The end is always near </html>

        ;; Experimental
        ;;module-eshell      ; for inferior OSes *cough*windows

        ;; Extra libraries
        extra-demo         ; allow me to demonstrate...
        extra-tags         ; if you liked it you should've generated a tag for it
        extra-tmux         ; close the rift between GUI & terminal
        extra-write        ; Emacs as a word processor

        ;; Personal
        my-commands
        my-bindings
        ))

;;
