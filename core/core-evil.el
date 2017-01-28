;;; core-evil.el

;; TODO Document

(defvar doom-evil-leader ","
  "The <leader> key, used by the `map!' macro for :leader bindings.")

(defvar doom-evil-localleader "\\"
  "The <localleader> key, used by the `map!' macro for :localleader bindings.")


;;
;; Packages
;;

(package! evil :demand t
  :config
  (setq evil-magic t
        evil-want-C-u-scroll t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-interactive-search-highlight 'selected-window
        evil-echo-state nil
        evil-ex-substitute-global t
        evil-insert-skip-empty-lines t
        evil-want-fine-undo nil

        evil-normal-state-tag    "N"
        evil-insert-state-tag    "I"
        evil-visual-state-tag    "V"
        evil-emacs-state-tag     "E"
        evil-operator-state-tag  "O"
        evil-motion-state-tag    "M"
        evil-replace-state-tag   "R")

  (evil-mode +1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (defpopup! ("*evil-registers*" :size 0.3)
             ("*Command Line*" :size 8))

  (mapc (lambda (r) (evil-set-initial-state (car r) (cdr r)))
        '((compilation-mode       . normal)
          (help-mode              . normal)
          (message-mode           . normal)
          (debugger-mode          . normal)
          (image-mode             . normal)
          (doc-view-mode          . normal)
          (eww-mode               . normal)
          (tabulated-list-mode    . emacs)
          (profile-report-mode    . emacs)
          (Info-mode              . emacs)
          (view-mode              . emacs)
          (comint-mode            . emacs)
          (cider-repl-mode        . emacs)
          (term-mode              . emacs)
          (calendar-mode          . emacs)
          (Man-mode               . emacs)
          (grep-mode              . emacs)))

  ;;; Macros
  (defsubst doom--evil-textobj! (key inner-fn &optional outer-fn)
    "Define a text object."
    (define-key evil-inner-text-objects-map key inner-fn)
    (define-key evil-outer-text-objects-map key (or outer-fn inner-fn)))

  ;; Shortcuts for the evil expression register
  (defmacro $= (str &rest args) `(calc-eval (format ,str ,@args)))
  (defmacro $r (char) `(evil-get-register ,char))
  (defmacro $expand (path) `(evil-ex-replace-special-filenames ,path))

  ;;; Evil hacks
  (defun doom*evil-esc ()
    "Disable search highlights and quit the minibuffer if open."
    (when (minibuffer-window-active-p (minibuffer-window))
      (abort-recursive-edit))
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)))
  (advice-add 'evil-force-normal-state :after 'doom*evil-esc)

  ;; Move to new split
  (defun doom*evil-window-follow (&rest _)  (evil-window-down 1))
  (defun doom*evil-window-vfollow (&rest _) (evil-window-right 1))
  (advice-add 'evil-window-split  :after 'doom*evil-window-follow)
  (advice-add 'evil-window-vsplit :after 'doom*evil-window-vfollow)

  ;; Fix harmless (yet disruptive) error reporting w/ hidden buffers caused by
  ;; workgroups killing windows
  ;; TODO Delete timer on dead windows?
  ;; (defun doom*ignore-errors (orig-fn &rest args)
  ;;   (ignore-errors (apply orig-fn args)))
  ;; (advice-add 'evil-ex-hl-do-update-highlight :around 'doom*ignore-errors)

  ;; Hide keystroke display while isearch is active
  (add-hook! isearch-mode     (setq echo-keystrokes 0))
  (add-hook! isearch-mode-end (setq echo-keystrokes 0.02))

  ;; monkey patch `evil-ex-replace-special-filenames' to add more ex
  ;; substitution flags to evil-mode
  (advice-add 'evil-ex-replace-special-filenames
              :override 'doom*evil-ex-replace-special-filenames)

  ;; Extra argument types for highlighting buffer (or global) regexp matches
  (evil-ex-define-argument-type buffer-match :runner doom-evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner doom-evil-ex-global-match)

  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match
    (list (when (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<g//>"
    :ex-arg global-match
    (when (evil-ex-p) (evil-ex-parse-global evil-ex-argument)))

  (evil-define-operator doom:evil-ex-global (beg end pattern command &optional invert)
    "Rewritten :g[lobal] that will highlight buffer matches. Takes the same arguments."
    :motion mark-whole-buffer :move-point nil
    (interactive "<r><g//><!>")
    (evil-ex-global beg end pattern command invert))

  (evil-define-operator doom:align (&optional beg end bang pattern)
    "Ex interface to `align-regexp'. Accepts vim-style regexps."
    (interactive "<r><!><//>")
    (align-regexp
     beg end
     (concat "\\(\\s-*\\)"
             (if bang
                 (regexp-quote pattern)
               (evil-transform-vim-style-regexp pattern)))
     1 1)))

(package! evil-anzu
  :init
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)

  ;; evil-anzu is strangely slow on startup. Byte compiling doesn't help. We use
  ;; this to lazy load it instead.
  (defun doom*evil-search (&rest _)
    (require 'evil-anzu)
    (advice-remove 'evil-ex-start-search 'doom*evil-search))
  (advice-add 'evil-ex-start-search :before 'doom*evil-search))

(package! evil-args
  :commands (evil-inner-arg evil-outer-arg evil-forward-arg evil-backward-arg
             evil-jump-out-args)
  :config (doom--evil-textobj! "a" 'evil-inner-arg 'evil-outer-arg))

(package! evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(package! evil-easymotion :defer 1
  :config
  (defvar doom--evil-snipe-repeat-fn)
  (evilem-default-keybindings "g SPC")
  (evilem-define (kbd "g SPC n") 'evil-ex-search-next)
  (evilem-define (kbd "g SPC N") 'evil-ex-search-previous)
  (evilem-define "gs" 'evil-snipe-repeat
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))
  (evilem-define "gS" 'evil-snipe-repeat-reverse
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))

  (setq doom--evil-snipe-repeat-fn
        (evilem-create 'evil-snipe-repeat
                       :bind ((evil-snipe-scope 'whole-buffer)
                              (evil-snipe-enable-highlight)
                              (evil-snipe-enable-incremental-highlight)))))

(package! evil-embrace
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  ;; Defuns
  (defun doom--embrace-get-pair (char)
    (acond ((cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))
            `(,(car it) . ,(cdr it)))
           ((assoc-default char embrace--pairs-list)
            (if (functionp (embrace-pair-struct-read-function it))
                (let ((pair (funcall (embrace-pair-struct-read-function it))))
                  `(,(car pair) . ,(cdr pair)))
              `(,(embrace-pair-struct-left it) . ,(embrace-pair-struct-right it))))
           (t `(,char . ,char))))

  (defun doom-embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (doom--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

  (defun doom-embrace-latex ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

  (defun doom-embrace-elisp-fn ()
    "Elisp function support for embrace."
    (cons (format "(%s " (or (read-string "(") "")) ")"))

  ;; Make embrace support escaped sequences
  (push (cons ?\\ (make-embrace-pair-struct
                   :key ?\\
                   :read-function 'doom-embrace-escaped
                   :left-regexp "\\[[{(]"
                   :right-regexp "\\[]})]"))
        (default-value 'embrace--pairs-list))

  ;; Add extra pairs
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" 'doom-embrace-elisp-fn))
  (add-hook! (org-mode latex-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" 'doom-embrace-latex)))

(package! evil-escape
  :commands evil-escape-mode
  :init
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)

  ;; evil-escape causes noticable lag in commands that start with j, so we
  ;; enable it only where we need it.
  (defun doom|evil-escape-disable () (evil-escape-mode -1))
  (defun doom|evil-escape-enable ()  (evil-escape-mode +1))
  (add-hook 'evil-insert-state-entry-hook 'doom|evil-escape-enable)
  (add-hook 'evil-insert-state-exit-hook  'doom|evil-escape-disable)
  (add-hook 'evil-replace-state-entry-hook 'doom|evil-escape-enable)
  (add-hook 'evil-replace-state-exit-hook  'doom|evil-escape-disable))

(package! evil-exchange
  :commands evil-exchange
  :config
  (defun doom*evil-exchange-off ()
    (when evil-exchange--overlays (evil-exchange-cancel)))
  (advice-add 'evil-force-normal-state :after 'doom*evil-exchange-off))

(package! evil-indent-plus
  :commands (evil-indent-plus-i-indent
             evil-indent-plus-a-indent
             evil-indent-plus-i-indent-up
             evil-indent-plus-a-indent-up
             evil-indent-plus-i-indent-up-down
             evil-indent-plus-a-indent-up-down)
  :config
  (doom--evil-textobj! "i" 'evil-indent-plus-i-indent 'evil-indent-plus-a-indent)
  (doom--evil-textobj! "I" 'evil-indent-plus-i-indent-up 'evil-indent-plus-a-indent-up)
  (doom--evil-textobj! "J" 'evil-indent-plus-i-indent-up-down 'evil-indent-plus-a-indent-up-down))

(package! evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config
  (global-evil-matchit-mode 1)
  (doom--evil-textobj! "%" 'evilmi-text-object)

  (defun doom/evil-matchit-or-toggle-fold ()
    "If on a fold-able element, toggle the fold (`hs-toggle-hiding'). Otherwise,
if on a delimiter, jump to the matching one (`evilmi-jump-items')."
    (interactive)
    (if (ignore-errors (hs-already-hidden-p))
        (hs-toggle-hiding)
      (call-interactively 'evilmi-jump-items))))

(package! evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match)
  :config (evil-multiedit-default-keybinds))

(package! evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(package! evil-textobj-anyblock
  :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block)
  :init (doom--evil-textobj! "B" 'evil-textobj-anyblock-inner-block 'evil-textobj-anyblock-a-block))

(package! evil-search-highlight-persist :demand t
  :config
  (global-evil-search-highlight-persist t)
  (advice-add 'evil-force-normal-state :after 'evil-search-highlight-persist-remove-all))

(package! evil-snipe :demand t
  :config
  (setq evil-snipe-smart-case t
        evil-snipe-repeat-keys nil ; using space to repeat
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-override-evil-repeat-keys nil ; causes problems with remapped ;
        evil-snipe-char-fold t
        evil-snipe-aliases '((?\[ "[[{(]")
                             (?\] "[]})]")
                             (?\; "[;:]")))
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)

  ;; Switch to evil-easymotion/avy after first snipe
  (map! :map evil-snipe-parent-transient-map
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively doom--evil-snipe-repeat-fn))))

(package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(package! evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config (global-evil-visualstar-mode 1))

(provide 'core-evil)
;;; core-evil.el ends here
