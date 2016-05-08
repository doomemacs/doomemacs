;;; core-ui.el --- interface & mode-line config

(setq-default
 blink-matching-paren nil ; don't blink--too distracting
 show-paren-delay 0.075

 ;; Keep cursors and highlights in current window only
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil

 uniquify-buffer-name-style nil ; my mode-line does this for me
 visible-bell nil
 visible-cursor nil
 x-stretch-cursor t
 use-dialog-box nil            ; always avoid GUI
 redisplay-dont-pause t        ; don't pause display on input
 indicate-buffer-boundaries t  ; show indicators where buffer starts/ends
 indicate-empty-lines t        ; show indicators on empty lines
 fringes-outside-margins t     ; switches order of fringe and margin
 split-width-threshold nil     ; favor horizontal splits
 show-help-function nil        ; hide :help-echo text

 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil

 ;; Disable bidirectional text support for slight performance bonus
 bidi-display-reordering nil

 ;; Minibuffer resizing
 resize-mini-windows 'grow-only
 max-mini-window-height 0.3

 ;; Remove arrow on the right fringe when wrapped
 fringe-indicator-alist (delq (assoc 'continuation fringe-indicator-alist)
                              fringe-indicator-alist))

(fset 'yes-or-no-p 'y-or-n-p)  ; y/n instead of yes/no

;; Ask for confirmation on exit only if there are real buffers left
(when window-system
  (setq confirm-kill-emacs
        (lambda (_)
          (if (narf/get-real-buffers)
              (y-or-n-p ">> Gee, I dunno Brain... Are you sure?")
            t))))

(tooltip-mode -1)  ; show tooltips in echo area
;; set up minibuffer and fringe
(if (not window-system)
    (menu-bar-mode -1)
  (scroll-bar-mode -1)  ; no scrollbar
  (tool-bar-mode   -1)  ; no toolbar
  ;; full filename in frame title
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; set fonts
  (set-frame-font (setq narf-current-font narf-default-font) t)
  (set-face-attribute 'default t :font narf-current-font)
  ;; standardize fringe width
  (fringe-mode narf-fringe-size)
  ;; Show tilde in margin on empty lines
  (define-fringe-bitmap 'tilde [64 168 16] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
  (set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
  ;; Brighter minibuffer when active + no fringe in minibuffer
  (defface narf-minibuffer-active '((t (:inherit mode-line)))
    "Face for active minibuffer")
  (add-hook! minibuffer-setup
    (set-window-fringes (selected-window) 0 0 nil)
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist '(default narf-minibuffer-active)))
  (add-hook! 'after-init-hook (set-window-fringes (minibuffer-window) 0 0 nil)))

;; Try to display unicode characters without upsetting line-hieght (as much as possible)
(defun narf-fix-unicode (set)
  (let ((font  (car set))
        (chars (cadr set))
        (size  (caddr set)))
    (mapc (lambda (x) (set-fontset-font
                  "fontset-default" `(,x . ,x)
                  (font-spec :name font :size size) nil 'prepend))
          chars)))
(mapc 'narf-fix-unicode '(("DejaVu Sans" (?⚠ ?★ ?λ ?➊ ?➋ ?➌ ?➍ ?➎ ?❻ ?➐ ?➑ ?➒ ?➓))))

;; on by default in Emacs 25; I prefer to enable on a mode-by-mode basis, so disable it
(when (and (> emacs-major-version 24) (featurep 'eldoc))
  (global-eldoc-mode -1))

;; Highlight TODO/FIXME/NOTE tags
(defface narf-todo-face  '((t (:inherit font-lock-warning-face))) "Face for TODOs")
(defface narf-fixme-face '((t (:inherit font-lock-warning-face))) "Face for FIXMEs")
(defface narf-note-face  '((t (:inherit font-lock-warning-face))) "Face for NOTEs")
(add-hook! (prog-mode emacs-lisp-mode)
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\((.+)\\)?:?\\)"  1 'narf-todo-face prepend)
         ("\\<\\(FIXME\\((.+)\\)?:?\\)" 1 'narf-fixme-face prepend)
         ("\\<\\(NOTE\\((.+)\\)?:?\\)"  1 'narf-note-face prepend))))

;; Fade out when unfocused
(add-hook! focus-in  (set-frame-parameter nil 'alpha 100))
(add-hook! focus-out (set-frame-parameter nil 'alpha 85))

;; Hide mode-line in help/compile window
(add-hook 'help-mode-hook 'narf|hide-mode-line)
(add-hook 'compilation-mode-hook 'narf|hide-mode-line)


;;
;; Plugins
;;

(use-package hl-line
  :init
  (add-hook! (prog-mode markdown-mode) 'hl-line-mode)
  :config
  (defvar-local narf--hl-line-mode nil)
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  (defun narf|hl-line-on ()  (if narf--hl-line-mode (hl-line-mode +1)))
  (defun narf|hl-line-off () (if narf--hl-line-mode (hl-line-mode -1)))

  (add-hook! hl-line-mode (if hl-line-mode (setq narf--hl-line-mode t)))
  ;; Disable line highlight in visual mode
  (add-hook 'evil-visual-state-entry-hook 'narf|hl-line-off)
  (add-hook 'evil-visual-state-exit-hook  'narf|hl-line-on))

(use-package visual-fill-column :defer t
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-width fill-column))

(use-package highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode)
  :init
  (add-hook! (nxml-mode yaml-mode json-mode scss-mode
              c-mode-common ruby-mode python-mode lua-mode)
    'highlight-indentation-mode)

  (after! editorconfig
    (defun narf*hl-indent-guess-offset ()
      (string-to-int (gethash 'indent_size (editorconfig-get-properties))))
    (advice-add 'highlight-indentation-guess-offset :override 'narf*hl-indent-guess-offset))

  ;; A long-winded method for ensuring whitespace is maintained (so that
  ;; highlight-indentation-mode can display them consistently)
  (add-hook! highlight-indentation-mode
    (if highlight-indentation-mode
        (progn
          (narf/add-whitespace)
          (add-hook 'after-save-hook 'narf/add-whitespace nil t)
          (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
      (remove-hook 'after-save-hook 'narf/add-whitespace t)
      (remove-hook 'before-save-hook 'delete-trailing-whitespace t))))

(use-package highlight-numbers :commands (highlight-numbers-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook! (emacs-lisp-mode lisp-mode js2-mode scss-mode c-mode-common)
    'rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 4))

(use-package rainbow-mode
  :commands (rainbow-mode)
  :init
  (add-hook! (sass-mode scss-mode less-css-mode) 'rainbow-mode)
  ;; hl-line-mode and rainbow-mode don't play well together
  (add-hook! rainbow-mode
    (when narf--hl-line-mode
      (hl-line-mode (if rainbow-mode -1 1)))))

(use-package nlinum
  :commands nlinum-mode
  :preface
  (defface linum-highlight-face '((t (:inherit linum))) "Face for line highlights")
  (defvar narf--hl-nlinum-overlay nil)
  (defvar narf--hl-nlinum-line nil)
  (defvar nlinum-format "%4d ")
  (setq linum-format "%3d ")
  :init
  (add-hook!
    (markdown-mode prog-mode scss-mode web-mode conf-mode)
    'nlinum-mode)
  (add-hook! 'nlinum-mode-hook
    (add-hook 'post-command-hook 'narf|nlinum-hl-line nil t))
  :config
  (add-hook! nlinum-mode
    (setq nlinum--width
          (length (save-excursion (goto-char (point-max))
                                  (format-mode-line "%l"))))))


;;
;; Mode-line
;;

(use-package spaceline
  :init
  (defvar-local narf--env-version nil)
  (defvar-local narf--env-command nil)
  (defvar powerline-height 23)
  (defvar powerline-default-separator nil)

  :config
  (defface mode-line-is-modified nil "Face for mode-line modified symbol")
  (defface mode-line-buffer-file nil "Face for mode-line buffer file path")

  ;; Custom modeline segments
  (spaceline-define-segment *buffer-path
    (if buffer-file-name
        (let ((buffer-path (f-relative buffer-file-name (f-dirname (narf/project-root))))
              (max-length 50))
          (if (> (length buffer-path) max-length)
              (let ((path (reverse (split-string buffer-path "/")))
                    (output ""))
                (when (and path (equal "" (car path)))
                  (setq path (cdr path)))
                (while (and path (< (length output) (- max-length 4)))
                  (setq output (concat (car path) "/" output))
                  (setq path (cdr path)))
                (when path
                  (setq output (concat "../" output)))
                output)
            buffer-path))
      "%b")
    :face other-face
    :tight-right t)

  (spaceline-define-segment *buffer-modified
    (propertize
     (concat (when buffer-file-name
               (concat
                (if (buffer-modified-p) "[+]")
                (if (not (file-exists-p buffer-file-name)) "[!]")))
             (if buffer-read-only "[RO]"))
     'face 'mode-line-is-modified)
    :when (not (string-prefix-p "*" (buffer-name)))
    :skip-alternate t
    :tight t)

  (spaceline-define-segment *buffer-position
    "A more vim-like buffer position."
    (let ((start (window-start))
          (end (window-end))
          (pend (point-max)))
      (if (and (eq start 1)
               (eq end pend))
          ":All"
        (let ((perc (/ end 0.01 pend)))
          (cond ((= start 1) ":Top")
                ((>= perc 100) ":Bot")
                (t (format ":%d%%%%" perc))))))
    :tight t)

  (spaceline-define-segment *vc
    "Version control info"
    (substring vc-mode (+ 1 (length (symbol-name (vc-backend buffer-file-name)))))
    :when (and active vc-mode)
    :face other-face
    :tight-right t)

  ;; search indicators
  (defface mode-line-count-face nil "")
  (make-variable-buffer-local 'anzu--state)
  (spaceline-define-segment *anzu
    "Show the current match number and the total number of matches. Requires
anzu to be enabled."
    (let ((here anzu--current-position)
          (total anzu--total-matched))
      (format " %s/%d%s "
              here total
              (if anzu--overflow-p "+" "")))
    :face (if active 'mode-line-count-face 'mode-line-inactive)
    :when (evil-ex-hl-active-p 'evil-ex-search)
    :tight t)

  (spaceline-define-segment *iedit
    "Show the number of matches and what match you're on (or after). Requires iedit."
    (let ((this-oc (iedit-find-current-occurrence-overlay))
          (length  (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
      (format " %s/%s "
              (save-excursion
                (unless this-oc
                  (iedit-prev-occurrence)
                  (setq this-oc (iedit-find-current-occurrence-overlay)))
                (if this-oc
                    ;; NOTE: Not terribly reliable
                    (- length (-elem-index this-oc iedit-occurrences-overlays))
                  "-"))
              length))
    :when (bound-and-true-p iedit-mode)
    :tight t
    :face (if active 'mode-line-count-face 'mode-line-inactive))

  (spaceline-define-segment *evil-substitute
    "Show number of :s matches in real time."
    (let ((range (if evil-ex-range
                     (cons (car evil-ex-range) (cadr evil-ex-range))
                   (cons (line-beginning-position) (line-end-position))))
          (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
      (if pattern
          (format " %s matches "
                  (count-matches pattern (car range) (cdr range))
                  evil-ex-argument)
        " ... "))
    :when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
    :tight t
    :face (if active 'mode-line-count-face 'mode-line-inactive))

  (spaceline-define-segment *macro-recording
    "Show when recording macro"
    (format " %s ▶ " (char-to-string evil-this-macro))
    :when (and active defining-kbd-macro)
    :face highlight-face
    :tight t)

  (spaceline-define-segment *buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (format "%s" buffer-file-coding-system)
    :when (not (string-match-p "\\(utf-8\\|undecided\\)"
                               (symbol-name buffer-file-coding-system))))

  (spaceline-define-segment *major-mode
    "The major mode, including process, environment and text-scale info."
    (concat
     (format "%s" mode-name)
     (if (stringp mode-line-process) mode-line-process)
     (if narf--env-version (concat " " narf--env-version))
     (and (featurep 'face-remap)
          (/= text-scale-mode-amount 0)
          (format " (%+d)" text-scale-mode-amount)))
    :tight-right t)

  (spaceline-define-segment *selection-info
    "Information about the current selection."
    (let ((reg-beg (region-beginning))
          (reg-end (region-end))
          (evil (eq 'visual evil-state)))
      (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
            (chars (- (1+ reg-end) reg-beg))
            (cols (1+ (abs (- (evil-column reg-end)
                              (evil-column reg-beg))))))
        (cond
         ;; rectangle selection
         ((or (bound-and-true-p rectangle-mark-mode)
              (and evil (eq 'block evil-visual-selection)))
          (format "%dx%dB" lines (if evil cols (1- cols))))
         ;; line selection
         ((or (> lines 1) (eq 'line evil-visual-selection))
          (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
              (format "%dL" lines)
            (format "%dC %dL" chars lines)))
         (t (format "%dC" (if evil chars (1- chars)))))))
    :when (evil-visual-state-p)
    :face highlight-face)

  ;; flycheck
  (defun narf--flycheck-count (state)
    "Return flycheck information for the given error type STATE."
    (when (flycheck-has-current-errors-p state)
      (if (eq 'running flycheck-last-status-change)
          "?"
        (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

  (defface spaceline-flycheck-error
    '((t (:foreground "#FC5C94" :distant-foreground "#A20C41")))
    "Face for flycheck error feedback in the modeline.")
  (defface spaceline-flycheck-warning
    '((t (:foreground "#F3EA98" :distant-foreground "#968B26")))
    "Face for flycheck warning feedback in the modeline.")
  (defface spaceline-flycheck-info
    '((t (:foreground "#8DE6F7" :distant-foreground "#21889B")))
    "Face for flycheck info feedback in the modeline.")

  (defvar-local narf--flycheck-err-cache nil "")
  (defvar-local narf--flycheck-cache nil "")
  (spaceline-define-segment *flycheck
    "Persistent and cached flycheck indicators in the mode-line."
    (or (and (or (eq narf--flycheck-err-cache narf--flycheck-cache)
                 (memq flycheck-last-status-change '(running not-checked)))
             narf--flycheck-cache)
        (and (setq narf--flycheck-err-cache flycheck-current-errors)
             (setq narf--flycheck-cache
                   (let ((fe (narf--flycheck-count 'error))
                         (fw (narf--flycheck-count 'warning))
                         ;; (fi (narf--flycheck-count 'info))
                         )
                     (concat
                      (if fe (propertize (format " ⚠%s " fe) 'face (if active 'spaceline-flycheck-error 'mode-line)))
                      (if fw (propertize (format " ⚠%s " fw) 'face (if active 'spaceline-flycheck-warning 'mode-line)))
                      ;; (if fi (propertize (format " ⚠%s " fi) 'face 'spaceline-flycheck-info))
                      )))))
    :when (and (bound-and-true-p flycheck-mode)
               (or flycheck-current-errors
                   (eq 'running flycheck-last-status-change)))
    :tight t)

  (defvar narf--mode-line-padding (pl/percent-xpm powerline-height 100 0 100 0 1 nil nil))
  (spaceline-define-segment *pad
    "Padding, to ensure the mode-line is `powerline-height' pixels tall"
    narf--mode-line-padding
    :tight-right t)

  (defun narf-spaceline-init ()
    (spaceline-install
     ;; Left side
     '(((*macro-recording *anzu *iedit *evil-substitute *flycheck)
        :skip-alternate t
        :fallback ("%I" :tight-right t))
       *buffer-path
       *buffer-modified
       *vc
       )
     ;; Right side
     '((*selection-info :when active)
       (*major-mode *env-version)
       *buffer-encoding-abbrev
       (global :when active)
       ("%l/%c" *buffer-position)
       *pad
       )))

  ;; Initialize modeline
  (narf-spaceline-init))

(provide 'core-ui)
;;; core-ui.el ends here
