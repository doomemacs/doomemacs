;;; init.el -*- lexical-binding: t; -*-
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/doom-emacs
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

;;; Syntax
;; Auto-Complete
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 1)

;;; UI
;; remove blinking cursor
(remove-hook 'doom-post-init-hook #'blink-cursor-mode)
(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)
(blink-cursor-mode -1)

;; remove mouse
(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter ""
  :keymap (make-sparse-keymap))

(dolist (type '(mouse down-mouse drag-mouse
                      double-mouse triple-mouse))
  (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
    ;; Yes, I actually HAD to go up to 7 here.
    (dotimes (n 7)
      (let ((k (format "%s%s-%s" prefix type n)))
        (define-key disable-mouse-mode-map
          (vector (intern k)) #'ignore)))))
(disable-mouse-mode 1)

(setq expand-file-name "~/Documents/org")
(setq org-directory "~/Documents/org")
(setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")

(setq +doom-modeline-height 56)
(setq +doom-modeline-bar-width 4)
(setq org-ellipsis " ▼ ")
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Fira Mono" :size 44)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 48)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 48)
      ivy-posframe-font (font-spec :family (font-get doom-font :family) :size 45)
      doom-big-font (font-spec :family "Anonymous Pro" :size 48))

(doom! :feature
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       eval              ; run code, run (also, repls)
       evil              ; come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       services          ; TODO managing external services & code builders
       syntax-checker    ; tasing you for every semicolon you forget
       version-control   ; remember, remember that commit in November
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       company           ; the ultimate code completion backend
       ;helm
       ivy               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       doom-modeline     ; a snazzy Atom-inspired mode-line
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       evil-goggles      ; display visual hints when editing in evil
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       neotree
       ;posframe          ; use child frames where possible (Emacs 26+ only)

       :tools
       dired             ; making dired pretty [functional]
       imenu             ; an imenu sidebar and searchable code index
       make              ; run make tasks from Emacs
       term              ; terminals in Emacs

       :lang
       cc                ; C/C++/Obj-C madness
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export)         ; Exporting org to whatever you want
       python             ; beautiful is better than ugly
       sh                 ; she sells (ba|z)sh shells on the C xor

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
      ; (write            ; emacs as a word processor (latex + org + markdown)
      ;  +wordnut         ; wordnet (wn) search
      ;  +langtool)       ; a proofreader (grammar/style check) for Emacs

       :config
       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +snippets +evil-commands)

       ;; This allows you to store your private module at
       ;; $XDG_CONFIG_HOME/doom/. Without +xdg it uses ~/.doom.d/. If your
       ;; config directory doesn't exist, this module does nothing.
       (private +xdg))
