;;; core-evil.el --- the root of all evil
;; see lib/evil-defuns.el

(use-package evil
  :init
  ;; highlight matching delimiters where it's important
  (defun show-paren-mode-off () (show-paren-mode -1))
  (add-hook! evil-insert-state-entry    'show-paren-mode)
  (add-hook! evil-insert-state-exit     'show-paren-mode-off)
  (add-hook! evil-visual-state-entry    'show-paren-mode)
  (add-hook! evil-visual-state-exit     'show-paren-mode-off)
  (add-hook! evil-motion-state-entry    'show-paren-mode)
  (add-hook! evil-motion-state-exit     'show-paren-mode-off)
  (add-hook! evil-operator-state-entry  'show-paren-mode)
  (add-hook! evil-operator-state-exit   'show-paren-mode-off)

  ;; Disable highlights on insert-mode
  (add-hook! evil-insert-state-entry 'evil-ex-nohighlight)
  :config
  (setq-default
   evil-magic t
   evil-want-C-u-scroll t       ; enable C-u for scrolling
   evil-ex-visual-char-range t  ; column range for ex commands
   evil-want-visual-char-semi-exclusive t
   evil-ex-search-vim-style-regexp t
   evil-ex-interactive-search-highlight 'selected-window
   evil-echo-state nil
   evil-ex-substitute-global t
   evil-insert-skip-empty-lines t

   evil-normal-state-tag    "N"
   evil-insert-state-tag    "I"
   evil-visual-state-tag    "V"
   evil-emacs-state-tag     "E"
   evil-operator-state-tag  "O"
   evil-motion-state-tag    "M"
   evil-replace-state-tag   "R"
   evil-iedit-state-tag     "R+"

   ;; Color-coded state cursors
   evil-default-cursor "orange"
   evil-normal-state-cursor  'box
   evil-emacs-state-cursor   '("cyan" box)
   evil-insert-state-cursor  'bar
   evil-visual-state-cursor  'hollow
   evil-iedit-state-cursor   'box)

  ;; NOTE: a bug in emacs 25 breaks undoing in evil. See
  ;; https://bitbucket.org/lyro/evil/issues/594/undo-doesnt-behave-like-vim
  (setq-default evil-want-fine-undo (if (> emacs-major-version 24) 'fine 'no))

  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (evil-define-key 'normal evil-command-window-mode-map [escape] 'kill-buffer-and-window)

  ;; modes to map to different default states
  (dolist (mode-map '((compilation-mode       . normal)
                      (help-mode              . normal)
                      (message-mode           . normal)
                      (debugger-mode          . normal)
                      (profile-report-mode    . emacs)

                      (Info-mode              . emacs)
                      (view-mode              . emacs)
                      (comint-mode            . emacs)
                      (cider-repl-mode        . emacs)
                      (term-mode              . emacs)
                      (calendar-mode          . emacs)
                      (Man-mode               . emacs)
                      (grep-mode              . emacs)
                      (image-mode             . normal)
                      ))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

  ;; Shortcuts for the evil expression register
  (defmacro $= (str &rest args)
    `(calc-eval (format ,str ,@args)))
  (defmacro $r (char)
    `(evil-get-register ,char))
  (defmacro $expand (path)
    `(evil-ex-replace-special-filenames ,path))

  ;; buffer-local ex commands, thanks to:
  ;; http://emacs.stackexchange.com/questions/13186
  (defun evil-ex-define-cmd-local (cmd function)
    "Locally binds the function FUNCTION to the command CMD."
    (unless (local-variable-p 'evil-ex-commands)
      (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
    (evil-ex-define-cmd cmd function))
  ;; Shortcuts for `evil-ex-define-cmd'
  (defalias 'exmap  'evil-ex-define-cmd)
  (defalias 'exmap! 'evil-ex-define-cmd-local)

  (progn ; evil hacks
    (defadvice evil-force-normal-state (after evil-esc-quit activate)
      "Close popups, disable search highlights and quit the minibuffer if open."
      (when (minibuffer-window-active-p (minibuffer-window))
        (narf-minibuffer-quit))
      (ignore-errors
        (evil-ex-nohighlight))
      (unless (bound-and-true-p org-src-mode)
        (narf/popup-close)))

    ;; Fix disruptive errors w/ hidden buffers caused by workgroups killing windows
    ;; TODO Delete timer on dead windows
    (defadvice evil-ex-hl-do-update-highlight (around evil-ex-hidden-buffer-ignore-errors activate)
      (ignore-errors ad-do-it))

    ;; Monkey-patch an error triggered randomly during column-selection caused
    ;; by `evil-move-to-column' receiving a float:
    ;;   evil-move-to-column: Wrong type argument: wholenump, 12.0
    (defun narf*evil-move-to-column-fix (args)
      (mapcar (lambda (i) (if (numberp i) (truncate i) i)) args))
    (advice-add 'evil-move-to-column :filter-args 'narf*evil-move-to-column-fix)
    (advice-add 'extract-rectangle-line :filter-args 'narf*evil-move-to-column-fix)

    ;; Hide keystroke display while isearch is active
    (add-hook! isearch-mode     (setq echo-keystrokes 0))
    (add-hook! isearch-mode-end (setq echo-keystrokes 0.02))
    (let ((map evil-ex-search-keymap))
      (define-key map "\C-w" 'backward-kill-word)
      (define-key map "\C-u" 'evil-delete-whole-line))

    ;; Repeat motions with SPC/S-SPC
    (defmacro narf-space-setup! (command next-func prev-func)
      `(defadvice ,command
           (before ,(intern (format "narf-space--%s" (symbol-name command))) activate)
         (define-key evil-motion-state-map (kbd "SPC") ',next-func)
         (define-key evil-motion-state-map (kbd "S-SPC") ',prev-func)))

    (after! evil-snipe
      (narf-space-setup! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

    (after! evil-visualstar
      (narf-space-setup! evil-visualstar/begin-search-forward evil-ex-search-next evil-ex-search-previous)
      (narf-space-setup! evil-visualstar/begin-search-backward evil-ex-search-previous evil-ex-search-next))

    (narf-space-setup! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
    (narf-space-setup! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)

    (narf-space-setup! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
    (narf-space-setup! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

    ;; A monkey patch to add several substitutions to evil-mode's ex commandline
    ;; other than % and #...
    ;;
    ;;   % => full path to file (/project/src/thing.c)
    ;;   # => alternative file path (/project/include/thing.h)
    ;;   %:p => path to project root (/project/)
    ;;   %:d => path to current directory (/project/src/)
    ;;   %:e => the file's extension (c)
    ;;   %:r => the full path without its extension (/project/src/thing)
    ;;   %:t => the file's basename (thing.c)
    ;;
    ;; Requires projectile (https://github.com/bbatsov/projectile) for
    ;; project-awareness, and f.el (https://github.com/rejeep/f.el) for file
    ;; functions.
    (defun evil-ex-replace-special-filenames (file-name)
      "Replace special symbols in FILE-NAME."
      (let ((current-fname (buffer-file-name))
            (alternate-fname (and (other-buffer)
                                  (buffer-file-name (other-buffer)))))
        (setq file-name
              ;; %:p:h => the project root (or current directory otherwise)
              (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:p\\)"
                                        (narf/project-root) file-name t t 2))
        (setq file-name
              ;; %:p => the current directory
              (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:d\\)"
                                        default-directory file-name t t 2))
        (when current-fname
          (setq file-name
                ;; %:e => ext
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:e\\)"
                                          (f-ext current-fname) file-name t t 2))
          (setq file-name
                ;; %:r => filename
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:r\\)"
                                          (f-no-ext current-fname) file-name t t 2))
          (setq file-name
                ;; %:t => filename.ext
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:t\\)"
                                          (f-base current-fname) file-name t t 2))
          (setq file-name
                ;; % => file path for current frame
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                          current-fname file-name t t 2)))
        (when alternate-fname
          (setq file-name
                ;; # => file path for alternative frame
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                          alternate-fname file-name t t 2)))
        (setq file-name
              (replace-regexp-in-string "\\\\\\([#%]\\)"
                                        "\\1" file-name t)))
      file-name))

  ;; Make :g[lobal] highlight matches
  (defvar narf-buffer-match-global evil-ex-substitute-global "")
  (defun narf--ex-buffer-match (flag &optional arg)
    (let ((hl-name 'evil-ex-buffer-match))
      (with-selected-window (minibuffer-selected-window)
        (narf/-ex-match-init hl-name)
        (narf/-ex-buffer-match arg hl-name (list (if narf-buffer-match-global ?g))))))
  (defun narf--ex-global-match (flag &optional arg)
    (let ((hl-name 'evil-ex-global-match))
      (with-selected-window (minibuffer-selected-window)
        (narf/-ex-match-init hl-name)
        (let ((result (car-safe (evil-ex-parse-global arg))))
          (narf/-ex-buffer-match result hl-name nil (point-min) (point-max))))))

  (evil-ex-define-argument-type buffer-match :runner narf--ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner narf--ex-global-match)

  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match
    (list (when (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<g//>"
    :ex-arg global-match
    (when (evil-ex-p) (evil-ex-parse-global evil-ex-argument)))

  (evil-define-operator narf:align (&optional beg end bang pattern)
    (interactive "<r><!><//>")
    (align-regexp
     beg end
     (concat "\\(\\s-*\\)"
             (if bang
                 (regexp-quote pattern)
               (rxt-pcre-to-elisp pattern)))
     1 1))
  (evil-define-operator narf:evil-ex-global (beg end pattern command &optional invert)
    :motion mark-whole-buffer
    :move-point nil
    (interactive "<r><g//><!>")
    (evil-ex-global beg end pattern command invert))
  (exmap "g[lobal]" 'narf:evil-ex-global))

;; evil plugins
(use-package evil-anzu
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 2
        anzu-search-threshold 500))

(use-package evil-args
  :commands (evil-inner-arg evil-outer-arg evil-forward-arg evil-backward-arg evil-jump-out-args)
  :init
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg))

(use-package evil-commentary
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package evil-exchange
  :commands evil-exchange
  :config
  (advice-add 'evil-force-normal-state :after 'narf*evil-exchange-off))

(use-package evil-iedit-state
  :functions (iedit-current-occurrence-string iedit-restrict-region)
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :config
  (advice-add 'evil-force-normal-state :after 'evil-iedit-state/quit-iedit-mode)
  (define-key evil-iedit-state-map (kbd "<escape>") 'evil-iedit-state/quit-iedit-mode)
  (define-key evil-visual-state-map (kbd "SPC") 'narf:iedit-restrict-to-region)
  (let ((map evil-iedit-state-map))
    ;; Don't interfere with evil-snipe
    (define-key map "s" nil)
    (define-key map "S" nil)

    (define-key map "V"  'evil-visual-line)
    (define-key map "C"  'evil-iedit-state/substitute) ; instead of s/S
    (define-key map "za" 'iedit-toggle-unmatched-lines-visible)))

(use-package evil-indent-plus
  :commands
  (evil-indent-plus-i-indent
   evil-indent-plus-a-indent
   evil-indent-plus-i-indent-up
   evil-indent-plus-a-indent-up
   evil-indent-plus-i-indent-up-down
   evil-indent-plus-a-indent-up-down)
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

(use-package evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config (global-evil-matchit-mode 1)
  :init
  (define-key evil-normal-state-map "%" #'evilmi-jump-items)
  (define-key evil-inner-text-objects-map "%" #'evilmi-text-object)
  (define-key evil-outer-text-objects-map "%" #'evilmi-text-object))

(use-package evil-easymotion
  :defer 1
  :config
  (evilem-default-keybindings "g SPC")
  (evilem-define (kbd "g SPC n") 'evil-ex-search-next)
  (evilem-define (kbd "g SPC N") 'evil-ex-search-previous)
  (evilem-define "gs" 'evil-snipe-repeat
    (lambda ()
      (save-excursion
        (ignore-errors
          (call-interactively #'evil-snipe-s))))
    nil
    ((evil-snipe-enable-highlight)
     (evil-snipe-enable-incremental-highlight)))

  (evilem-define "gS" 'evil-snipe-repeat-reverse
    (lambda ()
      (save-excursion
        (ignore-errors
          (call-interactively #'evil-snipe-s))))
    nil
    ((evil-snipe-enable-highlight)
     (evil-snipe-enable-incremental-highlight))))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-textobj-anyblock
  :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block)
  :init
  (define-key evil-inner-text-objects-map "B" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "B" 'evil-textobj-anyblock-a-block))

(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t)
  (advice-add 'evil-force-normal-state :after 'evil-search-highlight-persist-remove-all))

(use-package evil-snipe
  :init
  (setq-default
   evil-snipe-smart-case t
   evil-snipe-repeat-keys nil ; using space to repeat
   evil-snipe-scope 'line
   evil-snipe-repeat-scope 'visible
   evil-snipe-override-evil-repeat-keys nil ; causes problems with remapped ;
   evil-snipe-symbol-groups '((?\[ "[[{(]")
                              (?\] "[]})]")
                              (?\; "[;:]")))
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1)

  (add-hook! org-mode
    (mapc (lambda (p) (add-to-list 'evil-surround-pairs-alist p))
          '((?l . narf/evil-surround-latex))))

  (add-hook! emacs-lisp-mode
    (setq evil-surround-pairs-alist
          (cons '(?\` . ("`" . "*")) evil-surround-pairs-alist)))

  (add-hook! python-mode
    (setq evil-surround-pairs-alist
          (cons '(?d . ("\"\"\"" . "\"\"\"")) evil-surround-pairs-alist)))

  ;; Escaped surround characters
  (setq-default evil-surround-pairs-alist
                (cons '(?\\ . narf/evil-surround-escaped)
                      evil-surround-pairs-alist)))

(use-package evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode 1))

(provide 'core-evil)
;;; core-evil.el ends here
