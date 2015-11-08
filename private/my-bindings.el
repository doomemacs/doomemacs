;;; my-bindings.el

;; Minimalistic key mapping! Why go so far for this?
;; ...
;; Uh. Good question.

(eval-when-compile (require 'core-defuns))

(bind!
 -
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Global keymaps                     ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 "A-x"   'execute-extended-command
 "M-x"   'smex
 "M-X"   'smex-major-mode-commands
 "M-;"   'eval-expression
 "M-="   'text-scale-increase
 "M--"   'text-scale-decrease
 "M-/"   'evil-commentary-line
 "M-b"   'narf:build
 "M-t"   'helm-projectile-find-file
 "A-`"   'narf-switch-to-iterm
 "C-`"   'narf/popwin-toggle

 "M-w"   'evil-window-delete
 "M-W"   (λ (let ((data (assq (selected-frame) narf-wg-frames)))
              (if data
                  (progn (wg-delete-workgroup (wg-get-workgroup (cdr data)))
                         (delete-frame (car data)))
                (delete-frame))))

 "M-n"   (λ (switch-to-buffer (generate-new-buffer "*new*")))
 "M-N"   (λ (let ((nlinum-p (and (featurep 'nlinum)
                                 (memq 'nlinum--setup-window window-configuration-change-hook))))
              ;; Disable nlinum to fix elusive "invalid face linum" bug
              (remove-hook 'window-configuration-change-hook 'nlinum--setup-window t)
              (let ((frame (new-frame))
                    (frame-name (format "*new-%s*" (length narf-wg-frames))))
                (with-selected-frame frame
                  (wg-create-workgroup frame-name t)
                  (add-to-list 'narf-wg-frames (cons frame frame-name))))
              (when nlinum-p
                (add-hook 'window-configuration-change-hook 'nlinum--setup-window nil t))))

 ;; Simpler window navigation
 "C-j"  'evil-window-down
 "C-k"  'evil-window-up
 "C-h"  'evil-window-left
 "C-l"  'evil-window-right

 :m "M-j"  'narf-multi-next-line
 :m "M-k"  'narf-multi-previous-line

 :n "M-r"  'narf:eval-buffer
 :v "M-r"  'narf:eval-region
 :n "M-d"  'dash-at-point
 :n "M-o"  'narf/ido-find-file
 :n "M-O"  'narf/ido-find-project-file

 :m "M-1" (λ (narf:switch-to-workgroup-at-index 0))
 :m "M-2" (λ (narf:switch-to-workgroup-at-index 1))
 :m "M-3" (λ (narf:switch-to-workgroup-at-index 2))
 :m "M-4" (λ (narf:switch-to-workgroup-at-index 3))
 :m "M-5" (λ (narf:switch-to-workgroup-at-index 4))
 :m "M-6" (λ (narf:switch-to-workgroup-at-index 5))
 :m "M-7" (λ (narf:switch-to-workgroup-at-index 6))
 :m "M-8" (λ (narf:switch-to-workgroup-at-index 7))
 :m "M-9" (λ (narf:switch-to-workgroup-at-index 8))

 (:when IS-MAC
   ;; Textmate-esque indent shift left/right
   :i "M-["           (kbd "C-o m l C-o I DEL C-o ` l")
   :i "M-]"           (λ (evil-shift-right (point-at-bol) (point-at-eol)))

   "<A-left>"       'backward-word
   "<A-right>"      'forward-word
   "<M-backspace>"  'narf/backward-kill-to-bol-and-indent
   "A-SPC"          'just-one-space
   "M-a"            'mark-whole-buffer
   "M-c"            'evil-yank
   "M-s"            'save-buffer
   "M-v"            'clipboard-yank
   "M-q"            'evil-quit-all
   "M-z"            'undo
   "M-Z"            'redo
   "C-M-f"          'narf:toggle-fullscreen)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Local keymaps                      ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 :m ";" 'evil-ex

 ;; <leader>
 (:prefix ","
   :nv ","   (λ (if (narf/project-p) (helm-projectile-switch-to-buffer) (helm-buffers-list)))
   :nv "<"   'helm-mini
   :nv "."   'helm-find-files
   :nv ">"   'helm-projectile-find-file-in-known-projects
   :nv "/"   'helm-projectile-find-file
   :nv ";"   'helm-semantic-or-imenu
   :nv ":"   'helm-imenu-in-all-buffers

   :nv "]"   'helm-etags-select
   :nv "a"   'helm-projectile-find-other-file
   :nv "E"   (λ (in! narf-emacs-dir (helm-projectile-find-file)))
   :nv "n"   'narf/ido-find-org-file
   :nv "N"   'narf:org-search-files-or-headers
   :nv "m"   'helm-recentf
   :nv "M"   'helm-projectile-recentf  ; recent PROJECT files
   :nv "p"   'helm-projectile-switch-project
   :v  "="   'align-regexp
   :nv "r"   'emr-show-refactor-menu

   :nv "qq"  'evil-save-and-quit
   :nv "QQ"  'narf/kill-all-buffers-do-not-remember

   :n "oo"  'narf-open-with
   :n "ob"  (λ (narf-open-with "Google Chrome"))
   :n "of"  (λ (narf-open-with "Finder.app" default-directory))
   :n "oF"  (λ (narf-open-with "Finder.app" (narf/project-root)))
   :n "ou"  (λ (narf-open-with "Transmit"))
   :n "oU"  (λ (narf-open-with "Transmit" default-directory))
   :n "ol"  (λ (narf-open-with "LaunchBar"))
   :n "oL"  (λ (narf-open-with "LaunchBar" default-directory))
   :n "ot"  (λ (narf:tmux-chdir nil t))
   :n "oT"  'narf:tmux-chdir)

 ;; <localleader>
 (:prefix "\\"
   :nv "\\" 'narf/neotree-toggle
   :nv "|"  'narf-switch-to-iterm
   :nv "."  'narf/neotree-find
   :nv ";"  'narf/nlinum-toggle
   :nv "E"  'evil-emacs-state

   :n "]"   'next-buffer
   :n "["   'previous-buffer

   :n "c"   'narf/reset-theme
   :n "s"   (λ (narf:yas-snippets t))        ; ido snippets dir
   :n "g"   'diff-hl-diff-goto-hunk
   :n "e"   (λ (call-interactively 'flycheck-buffer) (flycheck-list-errors))
   :n "p"   'helm-show-kill-ring
   :n "b"   'helm-bookmarks
   :n "w"   'narf:helm-wg
   :n "W"   'narf:workgroup-display)

 :n "Y" "y$"
 :n "K" 'smart-up

 ;; Don't move cursor on indent
 :n "="   (λ (save-excursion (call-interactively 'evil-indent)))
 :v "="   'evil-indent

 :n "zr"  'narf/evil-open-folds
 :n "zm"  'narf/evil-close-folds
 :n "zx"  'narf:kill-real-buffer
 :n "zX"  'bury-buffer

 :n "]b"  'narf/next-real-buffer
 :n "[b"  'narf/previous-real-buffer
 :m "]g"  'diff-hl-next-hunk
 :m "[g"  'diff-hl-previous-hunk
 :m "]e"  'narf/flycheck-next-error
 :m "[e"  'narf/flycheck-previous-error
 ;; Switch workgroups
 :n "]w"  'narf:switch-to-workgroup-right
 :n "[w"  'narf:switch-to-workgroup-left
 :m "gt"  'narf:switch-to-workgroup-right
 :m "gT"  'narf:switch-to-workgroup-left

 ;; Increment/decrement number under cursor
 :n "g="  'evil-numbers/inc-at-pt
 :n "g-"  'evil-numbers/dec-at-pt

 :n "gR"  'narf:eval-buffer
 :n "gc"  'evil-commentary
 :n "gy"  'evil-commentary-yank
 :n "gx"  'evil-exchange
 :n "gr"  'narf:eval-region
 :v "gR"  'narf:eval-region-and-replace
 :m "gl"  'avy-goto-line
 :m "g]"  'smart-right
 :m "g["  'smart-left
 :no "g@" 'narf/evil-macro-on-all-lines

 :v "."   'evil-repeat

 ;; vnoremap < <gv
 :v "<"   (λ (evil-shift-left (region-beginning) (region-end))
             (evil-normal-state)
             (evil-visual-restore))
 ;; vnoremap > >gv
 :v ">"   (λ (evil-shift-right (region-beginning) (region-end))
             (evil-normal-state)
             (evil-visual-restore))

 ;; undo/redo for regions
 :nv "u"    'undo-tree-undo
 :nv "C-r"  'undo-tree-redo

 :v "*"   'evil-visualstar/begin-search-forward
 :v "#"   'evil-visualstar/begin-search-backward

 ;; paste from recent yank register; which isn't overwritten by deletes or
 ;; other operations.
 :v "P"   "\"0p"

 :v "S"   'evil-surround-region
 :v "R"   'evil-iedit-state/iedit-mode  ; edit all instances of marked region
 :v "v"   'er/expand-region
 :v "V"   'er/contract-region

 ;; aliases for %
 :m "%"   'evilmi-jump-items
 :m [tab] (λ (cond ((eq major-mode 'org-mode)
                    (org-cycle))
                   (t
                    (if (ignore-errors (hs-already-hidden-p))
                        (hs-toggle-hiding)
                      (call-interactively 'evilmi-jump-items)))))

 ;; Restore osx text objects
 :i "<A-backspace>" 'evil-delete-backward-word
 :i "<A-delete>"    (λ (evil-forward-word) (evil-delete-backward-word))

 ;; Newline magic
 :i "<backspace>"   'backward-delete-char-untabify
 :i "<M-backspace>" 'narf/backward-kill-to-bol-and-indent
 :i "<C-return>"    'evil-ret-and-indent

 ;; escape from insert mode (more responsive than using key-chord-define)
 :ir  "j"    'narf:exit-mode-maybe
 :ir  "J"    'narf:exit-mode-maybe
 :irv "C-g"  'evil-normal-state

 :o "s"      'evil-surround-edit
 :o "S"      'evil-Surround-edit

 :n "!"      'rotate-word-at-point
 :v "!"      'rotate-region
 :e [escape] 'evil-normal-state

 (:map evil-window-map ; prefix "C-w"
   "u"       'winner-undo
   "C-u"     'winner-undo
   "C-r"     'winner-redo
   "C-h"     'evil-window-left     ; don't accidentally invoke help
   "C-j"     'evil-window-down     ; don't accidentally invoke help
   "C-k"     'evil-window-up       ; don't accidentally invoke help
   "C-l"     'evil-window-right    ; don't accidentally invoke help

   "C-w"     'ace-window
   "C-S-w"   (λ (ace-window 4))    ; swap windows
   "C-C"     (λ (ace-window 16)))  ; delete windows

 ;; Vim omni-complete emulation
 :i "C-SPC"     'company-complete-common
 :i "C-x C-k"   'company-dict
 :i "C-x C-f"   'company-files
 :i "C-x C-]"   'company-tags
 :i "C-x s"     'company-ispell
 :i "C-x C-s"   'company-yasnippet
 :i "C-x C-o"   'company-semantic
 :i "C-x C-n"   'company-dabbrev-code
 :i "C-x C-p"   (λ (let ((company-selection-wrap-around t))
                     (call-interactively 'company-dabbrev-code)
                     (company-select-previous-or-abort)))

 (:after company
   (:map company-active-map
     "C-o"        'company-search-kill-others
     "C-n"        'company-select-next-or-abort
     "C-p"        'company-select-previous-or-abort
     "C-h"        'company-show-doc-buffer
     "C-S-h"      'company-show-location
     "C-S-s"      'company-search-candidates
     "C-s"        'company-filter-candidates
     "C-SPC"      'company-complete-common-or-cycle
     [tab]        'narf/company-complete-common-or-complete-full
     "<backtab>"  'company-select-previous
     [escape]     'company-abort
     "<C-return>" 'helm-company)
   (:map company-search-map
     "C-n"        'company-search-repeat-forward
     "C-p"        'company-search-repeat-backward
     [escape]     'company-abort))

 (:after help-mode
   (:map help-mode-map
     :n "]]" 'help-go-forward
     :n "[[" 'help-go-back
     :n "<escape>" (λ (kill-buffer)
                      (if (eq popwin:popup-buffer (current-buffer))
                          (popwin:close-popup-window)
                        (evil-window-delete)))))

 (:map evil-ex-completion-map
   "C-r"            'evil-ex-paste-from-register   ; registers in ex-mode
   "C-a"            'move-beginning-of-line
   "<s-left>"       'move-beginning-of-line
   "<s-right>"      'move-beginning-of-line
   "<s-backspace>"  'evil-delete-whole-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap fixes                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section is dedicated to keymaps that "fix" certain keys so
;; that they behave more like vim (or how I like it).

;; Restores "dumb" indentation to the tab key. This rustles a lot of
;; peoples' jimmies, apparently, but it's how I like it.
(bind! :i "<tab>"   'narf/dumb-indent
       :i "<C-tab>" 'indent-for-tab-command

       ;; No dumb-tab for lisp
       :i :map lisp-mode-map        [remap narf/dumb-indent] 'indent-for-tab-command
       :i :map emacs-lisp-mode-map  [remap narf/dumb-indent] 'indent-for-tab-command

       ;; Highjacks space/backspace to:
       ;;   a) delete spaces on either side of the cursor, if present ( | ) -> (|)
       ;;   b) allow backspace to delete space-indented blocks intelligently
       ;;   c) and not do any of this magic when inside a string
       :i "SPC"                                  'narf/inflate-space-maybe
       :i [remap backward-delete-char-untabify]  'narf/deflate-space-maybe
       :i [remap newline]                        'narf/newline-and-indent

       ;; Smarter move-to-beginning-of-line
       :i [remap move-beginning-of-line]         'narf/move-to-bol

       ;; Restore bash-esque keymaps in insert mode; C-w and C-a already exist
       :i "C-e" 'narf/move-to-eol
       :i "C-u" 'narf/backward-kill-to-bol-and-indent

       ;; Fixes delete
       :i "<kp-delete>" 'delete-char

       ;; Fix osx keymappings and then some
       :i "<M-left>"   'narf/move-to-bol
       :i "<M-right>"  'narf/move-to-eol
       :i "<M-up>"     'beginning-of-buffer
       :i "<M-down>"   'end-of-buffer
       :i "<C-up>"     'smart-up
       :i "<C-down>"   'smart-down

       ;; Fix emacs motion keys
       :i "A-b"      'evil-backward-word-begin
       :i "A-w"      'evil-forward-word-begin
       :i "A-e"      'evil-forward-word-end

       ;; Textmate-esque insert-line before/after
       :i "<M-return>"    'evil-open-below
       :i "<S-M-return>"  'evil-open-above
       ;; insert lines in-place)
       :n "<M-return>"    (λ (save-excursion (evil-insert-newline-below)))
       :n "<S-M-return>"  (λ (save-excursion (evil-insert-newline-above)))

       ;; Make ESC quit all the things
       :e [escape] 'narf-minibuffer-quit
       (:map (minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
         [escape] 'narf-minibuffer-quit)

       :map read-expression-map "C-w" 'evil-delete-backward-word)

(bind! :i "A-o" (λ (insert "ø"))
       :i "A-O" (λ (insert "Ø")))

;; Disable the global drag-mouse map; clicking in new buffers often sends evil
;; into visual mode, which is UN...ACCEPTAABBLLLEEEE!
(global-unset-key (kbd "<drag-mouse-1>"))

(provide 'my-bindings)
;;; my-bindings.el ends here
