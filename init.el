;;; init.el
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;; Version: 1.1.2
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

(defconst emacs-start-time (current-time))
(load (concat user-emacs-directory "core/core.el"))

(doom :default-theme  'doom-one
      :terminal-theme 'doom-dark
      :default-font ("Fira Mono" 12)

      ;;; The heart of DOOM
      core-popup         ; taming sudden and inevitable windows
      core-os            ; os-specific config
      core-scratch       ; a perdier scratch buffer
      core-ui            ; draw me like one of your French editors
      core-evil          ; come to the dark side, we have cookies
      core-editor        ; filling the editor-shaped hole in the emacs OS
      core-docs          ; getting to the documentation
      core-company       ; auto-completion, for the lazy typist
      core-yasnippet     ; snippets, for the lazier typist
      core-autoinsert    ; file templates, for the laziest typist
      core-flycheck      ; get tazed for every semicolon you forget
      core-project       ; for project navigation aficionados
      core-vcs           ; remember remember, that commit in November
      core-helm          ; a search engine for life and love
      core-workgroups    ; cure Emacs alzheimers + tab emulation
      core-eval          ; run code, run; debug too

      ;;; Dev environments
      module-cc          ; C/C++/Obj-C madness
      module-crystal     ; ruby at the speed of c
      module-csharp      ; unity, .NET, and mono shenanigans
      module-css         ; #big-bang::before { content: ""; }
      module-data        ; config/data formats
      module-db          ; using emacs as a db browser/client
      module-elisp       ; drowning in parentheses
      module-go          ; the hipster dialect
      module-haskell     ; a language that's lazier than I am
      module-java        ; the poster child for carpal tunnel syndrome
      module-js          ; all(hope(abandon(ye(who(enter(here))))))
      module-julia       ; MATLAB, but fast
      module-latex       ; for writing papers in Emacs
      module-lua         ; one-based indices? one-based indices.
      module-php         ; making php less painful to work with
      module-processing  ; pretty prototypes
      module-python      ; beautiful is better than ugly
      module-rest        ; Emacs as a service
      module-ruby        ; 1.step do {|i| p "Ruby is #{i&1==0?'love':'life'}"}
      module-rust        ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
      module-scala       ; Java, but good
      module-sh          ; she sells Z-shells by the C XOR
      module-swift       ; I wanted emoji variables. I got them
      module-text        ; writing docs for people to ignore
      module-web         ; The end is always near </html>

      ;;; Experimental
      ;; module-eshell    ; for inferior OSes *cough*windows
      ;; module-org       ; for organized fearless leader

      ;;; Extra libraries
      extra-demo         ; allow me to demonstrate...
      extra-tags         ; if you liked it you should've generated a tag for it
      extra-tmux         ; close the rift between GUI & terminal
      extra-write        ; Emacs as a word processor

      ;;; Personal
      my-commands
      my-bindings)

