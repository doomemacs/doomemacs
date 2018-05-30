;;; config/default/+emacs-bindings.el -*- lexical-binding: t; -*-

;; File that defines sensible key bindings for non-evil users

;; persp-mode and projectile in different prefixes
(setq persp-keymap-prefix (kbd "C-c e"))
(setq projectile-keymap-prefix (kbd "C-c p"))

(map!
 ;; Text scaling
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease
 ;; Editor related bindings
  "C-a"           #'doom/backward-to-bol-or-indent
 [remap newline]  #'newline-and-indent
 "C-S-s"          #'swiper
 ;; Buffer related bindings
 "C-x b"       #'persp-switch-to-buffer
 "C-x C-b"     #'ibuffer-list-buffers
 "C-x B"       #'switch-to-buffer
 "C-x k"       #'doom/kill-this-buffer-in-all-windows
 ;; Popup bindigns
 "C-x p"   #'+popup/other
 "C-`"     #'+popup/toggle
 "C-~"     #'+popup/raise
 ;; Doom emacs bindings
 (:prefix "C-c d"
   "d" #'+doom-dashboard/open
   "f" #'recentf-open-files
   "n" #'+neotree/open
   "o" #'+popup/other
   "t" #'+popup/toggle
   "c" #'+popup/close
   "C" #'+popup/close-all
   "r" #'+popup/raise
   "R" #'+popup/restore
   "s" #'doom/open-scratch-buffer)
 ;; Org related bindings
 (:prefix "C-c o"
   "s"     #'org-caldav-sync
   "a a"   #'org-agenda
   "a t"   #'org-todo-list
   "a m"   #'org-tags-view
   "a v"   #'org-search-view
   "c"     #'org-capture
   "C"     (λ! (require 'org-capture) (call-interactively #'org-capture-goto-target))
   "b"     #'org-iswitchb
   "e l b" #'org-beamer-export-to-latex
   "e l B" #'org-beamer-export-as-latex
   "e l P" #'org-beamer-export-to-pdf
   "l"     #'org-store-link)
 ;; Worspace management bindings
 (:prefix "C-c w"
   "d" #'+workspace/display
   "r" #'+workspace/rename
   "c" #'+workspace/new
   "k" #'+workspace/delete
   "s" #'+workspace/save-session
   "l" #'+workspace/load-session
   "o" #'doom/kill-other-buffers
   "u" #'winner-undo
   "U" #'winner-redo
   "p" #'+workspace/switch-left
   "n" #'+workspace/switch-right
   "1" (λ! (+workspace/switch-to 0))
   "2" (λ! (+workspace/switch-to 1))
   "3" (λ! (+workspace/switch-to 2))
   "4" (λ! (+workspace/switch-to 3))
   "5" (λ! (+workspace/switch-to 4))
   "6" (λ! (+workspace/switch-to 5))
   "7" (λ! (+workspace/switch-to 6))
   "8" (λ! (+workspace/switch-to 7))
   "9" (λ! (+workspace/switch-to 8))
   "0" #'+workspace/switch-to-last)
 ;; Version control bindings
  (:prefix "C-c v"
   "s" #'magit-status
   "i" #'+vcs/git-browse-issues
   "b" #'+vcs/git-browse)
  ;; Mail related bindings
  (:prefix "C-c m"
    "s" #'mail-add-attachment
    "m" #'mu4e
    "s" #'message-send)

 ;; Plugins

  ;; misc plugins
  "<f9>" #'+neotree/open
  "C-="  #'er/expand-region
  "C-'"  #'imenu-list-smart-toggle
 ;; smartparens
 "C-M-a" #'sp-beginning-of-sexp
 "C-M-e" #'sp-end-of-sexp
 "C-M-f" #'sp-forward-sexp
 "C-M-b" #'sp-backward-sexp
 "C-M-d" #'sp-splice-sexp
 ;; company mode
 "<C-tab>" #'+company/complete
 ;; Counsel Bindings
 "C-h b" #'counsel-descbinds
 ;; Repl Toggle
 "C-c C-z" #'+eval/open-repl

 ;; Restore common editing keys (and ESC) in minibuffer
 (:map (minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-completion-map
        minibuffer-local-must-match-map
        minibuffer-local-isearch-map
        read-expression-map)
   "C-g" #'abort-recursive-edit
   "C-a" #'move-beginning-of-line
   "C-/" #'doom/minibuffer-undo)
 ;; Company mode
 (:after company
   (:map company-active-map
     "C-o"        #'company-search-kill-others
     "C-n"        #'company-select-next
     "C-p"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     "<C-tab>"    #'company-complete-common-or-cycle
     [tab]        #'company-complete-common-or-cycle
     [backtab]    #'company-select-previous
     "C-g"        (λ! (company-abort))
     [C-return]   #'counsel-company)
 (:map company-search-map
   "C-n"        #'company-search-repeat-forward
   "C-p"        #'company-search-repeat-backward
   "C-s"        (λ! (company-search-abort) (company-filter-candidates))
   "C-g"        #'company-search-abort))
 ;; NeoTree bindings
 (:after neotree
   :map neotree-mode-map
   "q"       #'neotree-hide
   [return]  #'neotree-enter
   "RET"     #'neotree-enter
   "SPC"     #'neotree-quick-look
   "v"       #'neotree-enter-vertical-split
   "s"       #'neotree-enter-horizontal-split
   "c"       #'neotree-create-node
   "D"       #'neotree-delete-node
   "g"       #'neotree-refresh
   "r"       #'neotree-rename-node
   "R"       #'neotree-refresh
   "h"       #'+neotree/collapse-or-up
   "l"       #'+neotree/expand-or-open
   "n"       #'neotree-next-line
   "p"       #'neotree-previous-line
   "N"       #'neotree-select-next-sibling-node
   "P"       #'neotree-select-previous-sibling-node)
 (:after help-mode
   (:map help-mode-map
     "o" #'ace-link-help
     ">" #'help-go-forward
     "<" #'help-go-back))
 (:after helpful-mode
   (:map helpful-mode-map
     "o" #'ace-link-help))
 (:after info
   (:map Info-mode-map
     "o" #'ace-link-info))
 ;; Yasnippet
 (:after yasnippet
   ;; keymap while editing an inserted snippet
   (:map yas-keymap
     "C-e"           #'snippets/goto-end-of-field
     "C-a"           #'snippets/goto-start-of-field
     "<S-tab>"       #'yas-prev-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field))
 ;; Flycheck
 (:after flycheck
   (:map flycheck-error-list-mode-map
     "C-n" #'flycheck-error-list-next-error
     "C-p" #'flycheck-error-list-previous-error
     "RET" #'flycheck-error-list-goto-error))
 ;; ivy stuff
 (:after ivy
   (:map ivy-minibuffer-map
     "C-g" #'keyboard-escape-quit))
  (:after ein:notebook-multilang
   (:map ein:notebook-multilang-mode-map
     "C-c h" #'+ein/hydra/body))
 )


(which-key-add-key-based-replacements "C-c e"   "perspective")
(which-key-add-key-based-replacements "C-c d"   "doom")
(which-key-add-key-based-replacements "C-c d p" "doom popups")
(which-key-add-key-based-replacements "C-c m"   "mail")
(which-key-add-key-based-replacements "C-c o"   "org")
(which-key-add-key-based-replacements "C-c o a" "org agenda")
(which-key-add-key-based-replacements "C-c o e" "org export")
(which-key-add-key-based-replacements "C-c p"   "projects")
(which-key-add-key-based-replacements "C-c v"   "versioning")
(which-key-add-key-based-replacements "C-c w"   "workspace")
(which-key-add-key-based-replacements "C-c !"   "checking")
