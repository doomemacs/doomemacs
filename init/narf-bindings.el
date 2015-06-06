(eval-when-compile (require 'defuns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymaps                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind "M-x"      'smex
      "M-X"      'smex-major-mode-commands
      "M-A-x"    'helm-M-x
      "M-;"      'eval-expression
      "C-`"      'popwin:toggle-popup-window
      "M-="      'text-scale-increase
      "M--"      'text-scale-decrease
      "M-w"      'evil-window-delete
      "M-/"      'evil-commentary-line
      "M-b"      'narf::build
      "M-t"      'helm-projectile-find-file)

(bind motion
      ;; Faster scrolling
      "M-j"  "6j"
      "M-k"  "6k"
      "M-r"  'narf::eval

      normal
      "M-o"  'narf:ido-find-file
      "M-O"  'narf:ido-find-project-file
      "<f1>" 'dash-at-point

      "M-R"  'narf::eval-buffer)

;; restore text nav keys
(bind :if IS-MAC
      "<A-left>"       'backward-word
      "<A-right>"      'forward-word
      "<M-backspace>"  'narf:backward-kill-to-bol-and-indent
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

(bind (normal motion visual) ";" 'evil-ex)

(bind normal
      :prefix leader
      ","   (λ (if (narf/project-p) (helm-projectile-switch-to-buffer) (helm-buffers-list)))
      "<"   'helm-buffers-list
      "."   'narf:ido-find-file
      ">"   'narf:ido-find-file-other-window
      "/"   'helm-projectile-find-file
      ";"   'helm-semantic-or-imenu

      "]"   'helm-etags-select
      "a"   'helm-projectile-find-other-file
      "E"   'narf::initfiles
      "h"   'helm-apropos
      "n"   'narf::notes
      "m"   'helm-recentf
      "M"   'helm-projectile-recentf  ; recent PROJECT files
      "p"   'helm-projectile-switch-project
      "r"   'emr-show-refactor-menu

      "qq"  'evil-save-and-quit
      "QQ"  (λ (let ((confirm-kill-emacs nil)) (narf::kill-buffers t) (evil-quit-all)))

      ;; insert lines in-place
      "jj"  (λ (save-excursion (evil-insert-newline-below)))
      "kk"  (λ (save-excursion (evil-insert-newline-above)))

      "oo"  'narf:osx-open-with
      "ob"  (λ (narf:osx-open-with "Google Chrome"))
      "of"  (λ (narf:osx-open-with "Finder.app" default-directory))
      "oF"  (λ (narf:osx-open-with "Finder.app" (narf/project-root)))
      "ou"  (λ (narf:osx-open-with "Transmit"))
      "oU"  (λ (narf:osx-open-with "Transmit" default-directory))
      "ol"  (λ (narf:osx-open-with "LaunchBar"))
      "oL"  (λ (narf:osx-open-with "LaunchBar" default-directory))
      "ot"  (λ (narf::tmux-chdir nil t))   ; tmux: cd (default-directory)
      "oT"  'narf::tmux-chdir              ; tmux: cd [project root]

      :prefix localleader
      "\\"  'narf:neotree-toggle
      "."   'narf:neotree-find
      ";"   'narf:nlinum-toggle
      "="   'toggle-transparency
      "E"   'evil-emacs-state

      "]"   'next-buffer
      "["   'previous-buffer

      "s"   (λ (narf::snippets t))        ; ido snippets dir
      "g"   'diff-hl-diff-goto-hunk
      "e"   (λ (call-interactively 'flycheck-buffer) (flycheck-list-errors))
      "p"   'helm-show-kill-ring
      "b"   'helm-bookmarks
      "w"   'helm-wg)

(bind normal
      "Y"    (λ (evil-yank (point) (point-at-eol)))  ; yank to eol, like D and C

      "zr"   'narf:open-folds
      "zm"   'narf:close-folds

      "zx"   'narf:kill-real-buffer
      "zX"   'bury-buffer

      "]b"   'narf:next-real-buffer
      "[b"   'narf:previous-real-buffer
      "]w"   'wg-switch-to-workgroup-right
      "[w"   'wg-switch-to-workgroup-left

      ;; Increment/decrement number under cursor
      "g="   'evil-numbers/inc-at-pt
      "g-"   'evil-numbers/dec-at-pt
      "gR"   'narf::eval-buffer
      "gc"   'evil-commentary
      "gy"   'evil-commentary-yank

      visual
      "gR"   'narf::eval-region-and-replace
      ",="   'align-regexp

      ;; vnoremap < <gv
      "<"    (λ (evil-shift-left (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore))
      ;; vnoremap > >gv
      ">"    (λ (evil-shift-right (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore))

      ;; undo/redo for regions
      ;; "u"    'undo-tree-undo
      ;; "C-r"  'redo-tree-redo

      "*"    'evil-visualstar/begin-search-forward
      "#"    'evil-visualstar/begin-search-backward

      ;; paste from recent yank register; which isn't overwritten by deletes or
      ;; other operations.
      "P"    "\"0p"

      "S"   'evil-surround-region
      "R"   'evil-iedit-state/iedit-mode  ; edit all instances of marked region
      "v"   'er/expand-region
      "V"   'er/contract-region

      motion
      ;; aliases for %
      "%"     'evilmi-jump-items
      [tab]   (λ (if (ignore-errors (hs-already-hidden-p))
                     (hs-toggle-hiding)
                   (call-interactively 'evilmi-jump-items)))

      "j"    'evil-next-line
      "k"    'evil-previous-line

      "]g"    'diff-hl-next-hunk
      "[g"    'diff-hl-previous-hunk

      "]e"    (λ (call-interactively (if (bound-and-true-p flycheck-mode) 'flycheck-next-error 'next-error)))
      "[e"    (λ (call-interactively (if (bound-and-true-p flycheck-mode) 'flycheck-previous-error 'previous-error)))

      "gl"    'narf:goto-line
      "gs"    'evil-ace-jump-two-chars-mode
      "gx"    'evil-exchange
      "gr"    'narf::eval-region
      "g]"    'smart-down
      "g["    'smart-up

      insert
      "<A-backspace>" 'evil-delete-backward-word
      "<A-delete>"    (λ (evil-forward-word) (evil-delete-backward-word))

      ;; Newline magic
      "<backspace>"   'backward-delete-char-untabify
      "<M-backspace>" 'narf:backward-kill-to-bol-and-indent
      "<C-return>"    'evil-ret-and-indent

      ;; Textmate-esque indent shift left/right
      "M-["           (kbd "C-o m l C-o I DEL C-o ` l")
      "M-]"           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
      "<backtab>"     (kbd "M-[")

      ;; Company-mode
      "C-SPC"     'company-complete-common
      "C-x C-k"   'company-dictionary
      "C-x C-f"   'company-files
      "C-x C-]"   'company-tags
      "C-x s"     'company-ispell
      "C-x C-s"   'company-yasnippet
      "C-x C-o"   'company-semantic
      "C-x C-n"   'company-dabbrev-code
      "C-x C-p"   (λ (let ((company-selection-wrap-around t))
                       (call-interactively 'company-dabbrev-code)
                       (company-select-previous-or-abort)))

      (insert replace)
      ;; escape from insert mode (more responsive than using key-chord-define)
      "j"         'narf:exit-mode-maybe

      (insert replace visual)
      "C-g"       'evil-normal-state

      operator
      "s"   'evil-surround-edit
      "S"   'evil-Surround-edit

      ;; Rotate-text (see elisp/rotate-text.el)
      normal "!" 'rotate-word-at-point
      visual "!" 'rotate-region
      emacs  [escape] 'evil-normal-state)

(bind :map evil-window-map
      ;; winner-mode: window layout undo/redo (see core.el)
      "u"     'winner-undo
      "C-u"   'winner-undo
      "C-r"   'winner-redo

      "C-w"   'ace-window
      "C-S-w" (λ (ace-window 4))   ; swap windows
      "C-C"   (λ (ace-window 16))) ; delete windows

(after "help-mode"
  (bind normal :map help-mode-map
        "<escape>" (λ (kill-buffer)
                      (if (eq popwin:popup-buffer (current-buffer))
                          (popwin:close-popup-window)
                        (evil-window-delete)))
        "]]" 'help-go-forward
        "[[" 'help-go-back))

(bind :map evil-ex-completion-map
      "C-r"            'evil-ex-paste-from-register   ; registers in ex-mode
      "C-a"            'move-beginning-of-line
      "<s-left>"       'move-beginning-of-line
      "<s-right>"      'move-beginning-of-line
      "<s-backspace>"  'evil-delete-whole-line)

(bind :map company-active-map
      "C-o"        'company-search-kill-others
      "C-n"        'company-select-next-or-abort
      "C-p"        'company-select-previous-or-abort
      "C-h"        'company-show-doc-buffer
      "C-S-h"      'company-show-location
      "C-S-s"      'company-search-candidates
      "C-s"        'company-filter-candidates
      "C-SPC"      'company-complete-common
      [tab]        'company-complete
      "<backtab>"  'company-select-previous
      [escape]     'company-abort
      "<C-return>" 'helm-company
      "C-w"        nil

      :map company-search-map
      "C-n"        'company-search-repeat-forward
      "C-p"        'company-search-repeat-backward
      [escape]     'company-abort
      "C-w"        nil)

;; TODO: Swap helm's C-z and Tab


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap fixes                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section is dedicated to keymaps that "fix" certain keys so
;; that they behave more like vim (or how I like it).

;; Restores "dumb" indentation to the tab key. This rustles a lot of
;; peoples' jimmies, apparently, but it's how I like it.
(bind insert
      "<tab>"   'narf:dumb-indent
      "<C-tab>" 'indent-for-tab-command

      ;; No dumb-tab for lisp
      :map lisp-mode-map        [remap narf:dumb-indent] 'indent-for-tab-command
      :map emacs-lisp-mode-map  [remap narf:dumb-indent] 'indent-for-tab-command)

;; Highjacks space/backspace to:
;;   a) delete spaces on either side of the cursor, if present ( | ) -> (|)
;;   b) allow backspace to delete space-indented blocks intelligently
;;   c) and not do any of this magic when inside a string
(bind insert
      "SPC"                                  'narf:inflate-space-maybe
      [remap backward-delete-char-untabify]  'narf:deflate-space-maybe
      [remap newline]                        'narf:newline-and-indent

      ;; Smarter move-to-beginning-of-line
      [remap move-beginning-of-line]         'narf:move-to-bol

      ;; Restore bash-esque keymaps in insert mode; C-w and C-a already exist
      "C-e" 'narf:move-to-eol
      "C-u" 'narf:backward-kill-to-bol-and-indent

      ;; Fixes delete
      "<kp-delete>" 'delete-char

      ;; Fix osx keymappings and then some
      "<M-left>"   'narf:move-to-bol
      "<M-right>"  'narf:move-to-eol
      "<M-up>"     'beginning-of-buffer
      "<M-down>"   'end-of-buffer
      "<C-up>"     'smart-up
      "<C-down>"   'smart-down

      ;; Fix emacs motion keys
      "A-b"      'evil-backward-word-begin
      "A-w"      'evil-forward-word-begin
      "A-e"      'evil-forward-word-end

      (insert normal)
      ;; Textmate-esque insert-line before/after
      "<M-return>"    'evil-open-below
      "<S-M-return>"  'evil-open-above)

;; Make ESC quit all the things
(bind :map (minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map)
      [escape] 'narf/minibuffer-quit)
(bind emacs [escape] 'narf/minibuffer-quit)

(bind :map read-expression-map "C-w" 'evil-delete-backward-word)

;; Line selection via linum
(bind "<left-margin> <down-mouse-1>"    'narf/mouse-select-line
      "<left-margin> <mouse-1>"         'narf/mouse-select-line
      "<left-margin> <drag-mouse-1>"    'narf/mouse-select-line
      "<left-margin> <double-mouse-1>"  'narf/mouse-select-block)


(provide 'narf-bindings)
;;; narf-bindings.el ends here
