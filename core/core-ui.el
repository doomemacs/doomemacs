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
 use-dialog-box nil             ; always avoid GUI
 redisplay-dont-pause t         ; don't pause display on input
 indicate-buffer-boundaries nil ; don't show where buffer starts/ends
 indicate-empty-lines t         ; show empty lines
 fringes-outside-margins t      ; switches order of fringe and margin
 split-width-threshold nil      ; favor horizontal splits
 show-help-function nil         ; hide :help-echo text

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

(load-theme narf-current-theme t)
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
  (set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
  ;; Brighter minibuffer when active + no fringe in minibuffer
  (defface narf-minibuffer-active '((t (:inherit mode-line)))
    "Face for active minibuffer")
  (add-hook! minibuffer-setup
    (set-window-fringes (minibuffer-window) 0 0 nil)
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist '(default narf-minibuffer-active)))
  (add-hook! 'after-init-hook (set-window-fringes (minibuffer-window) 0 0 nil)))

;; Try to display unicode characters without upsetting line-hieght (as much as possible)
(narf-fix-unicode "DejaVu Sans" '(?⚠ ?★ ?λ ?➊ ?➋ ?➌ ?➍ ?➎ ?❻ ?➐ ?➑ ?➒ ?➓))

;; on by default in Emacs 25; I prefer to enable on a mode-by-mode basis, so disable it
(when (and (> emacs-major-version 24) (featurep 'eldoc))
  (global-eldoc-mode -1))

;; Highlight TODO/FIXME/NOTE tags
(defface narf-todo-face '((t (:inherit font-lock-warning-face)))
  "Face for TODOs")
(defface narf-fixme-face '((t (:inherit font-lock-warning-face)))
  "Face for FIXMEs")
(defface narf-note-face '((t (:inherit font-lock-warning-face)))
  "Face for NOTEs")
(add-hook! (prog-mode emacs-lisp-mode css-mode)
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'narf-todo-face prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'narf-fixme-face prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 'narf-note-face prepend))))

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
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :init
  (add-hook! (nxml-mode yaml-mode json-mode scss-mode
              c-mode-common ruby-mode python-mode lua-mode)
    'highlight-indentation-mode)

  (after! editorconfig
    (advice-add 'highlight-indentation-guess-offset
                :override 'narf*hl-indent-guess-offset))

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
  (add-hook! (emacs-lisp-mode lisp-mode js2-mode css-mode c-mode-common)
    'rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3))

(use-package rainbow-mode
  :commands (rainbow-mode)
  :init
  ;; hl-line-mode and rainbow-mode don't play well together
  (add-hook! (sass-mode scss-mode less-css-mode)
    '(rainbow-mode narf|hl-line-off)))

(use-package nlinum
  :commands nlinum-mode
  :preface
  (setq linum-format "%3d ")
  (defvar nlinum-format "%4d  ")
  (defvar narf--hl-nlinum-overlay nil)
  (defvar narf--hl-nlinum-line nil)
  (defface linum-highlight-face '((t (:inherit linum)))
    "Face for line highlights")
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
  (defface mode-line-buffer-file nil "Face for mode-line buffer file name")
  (defface mode-line-buffer-path nil "Face for mode-line buffer file path")

  ;; Custom modeline segments
  (spaceline-define-segment *buffer-path
    (concat
     (when buffer-file-name
       (powerline-raw
        (f-dirname
         (let ((buffer-path (f-relative buffer-file-name (narf/project-root)))
               (max-length (truncate (/ (window-body-width) 1.75))))
           (concat (projectile-project-name) "/"
                   (if (> (length buffer-path) max-length)
                       (let ((path (reverse (split-string buffer-path "/" t)))
                             (output ""))
                         (when (and path (equal "" (car path)))
                           (setq path (cdr path)))
                         (while (and path (<= (length output) (- max-length 4)))
                           (setq output (concat (car path) "/" output))
                           (setq path (cdr path)))
                         (when path
                           (setq output (concat "../" output)))
                         (when (string-suffix-p "/" output)
                           (setq output (substring output 0 -1)))
                         output)
                     buffer-path))))
        (if active 'mode-line-buffer-path)))
            (powerline-raw "%b" (if active 'mode-line-buffer-file)))
    :tight-right t)

  (spaceline-define-segment *buffer-modified
    (unless (string-prefix-p "*" (buffer-name))
      (powerline-raw
       (concat (when buffer-file-name
                 (concat
                  (if (buffer-modified-p) "[+]")
                  (if (not (file-exists-p buffer-file-name)) "[!]")))
               (if buffer-read-only "[RO]"))
       'mode-line-is-modified))
    :skip-alternate t
    :tight t)

  (spaceline-define-segment *buffer-position
    "A more vim-like buffer position."
    (concat "%l/%c "
            (let ((start (window-start))
                  (end (window-end))
                  (pend (point-max)))
              (if (and (eq start 1)
                       (eq end pend))
                  ":All"
                (let ((perc (/ end 0.01 pend)))
                  (cond ((= start 1) ":Top")
                        ((>= perc 100) ":Bot")
                        (t (format ":%d%%%%" perc)))))))
    :tight-left t)

  (spaceline-define-segment *vc
    "Version control info"
    (when (and active vc-mode)
      (powerline-raw
       (substring
        vc-mode (1+ (length (symbol-name (vc-backend buffer-file-name)))))
       other-face))
    :tight-right t)

  ;; search indicators
  (defface mode-line-count-face nil "")
  (make-variable-buffer-local 'anzu--state)
  (spaceline-define-segment *anzu
    "Show the current match number and the total number of matches. Requires
anzu to be enabled."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (powerline-raw
       (let ((here anzu--current-position)
             (total anzu--total-matched))
         (format " %s/%d%s "
                 here total
                 (if anzu--overflow-p "+" "")))
       (if active 'mode-line-count-face 'mode-line-inactive)))
    :tight t)

  (spaceline-define-segment *iedit
    "Show the number of matches and what match you're on (or after). Requires
iedit."
    (when (bound-and-true-p iedit-mode)
      (propertize
       (let ((this-oc (iedit-find-current-occurrence-overlay))
             (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
         (format
          " %s/%s "
          (save-excursion
            (unless this-oc
              (iedit-prev-occurrence)
              (setq this-oc (iedit-find-current-occurrence-overlay)))
            (if this-oc
                ;; NOTE: Not terribly reliable
                (- length (-elem-index this-oc iedit-occurrences-overlays))
              "-"))
          length))
       'face (if active 'mode-line-count-face 'mode-line-inactive)))
    :tight t)

  (spaceline-define-segment *evil-substitute
    "Show number of :s matches in real time."
    (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
      (powerline-raw
       (let ((range (if evil-ex-range
                        (cons (car evil-ex-range) (cadr evil-ex-range))
                      (cons (line-beginning-position) (line-end-position))))
             (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
         (if pattern
             (format " %s matches "
                     (count-matches pattern (car range) (cdr range))
                     evil-ex-argument)
           " ... "))
       (if active 'mode-line-count-face 'mode-line-inactive)))
    :tight t)

  (spaceline-define-segment *macro-recording
    "Show when recording macro"
    (when (and active defining-kbd-macro)
      (powerline-raw
       (format " %s ▶ " (char-to-string evil-this-macro))
       highlight-face))
    :tight t)

  (spaceline-define-segment *buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (unless (string-match-p "\\(utf-8\\|undecided\\)"
                            (symbol-name buffer-file-coding-system))
      (format "%s" buffer-file-coding-system)))

  (spaceline-define-segment *major-mode
    "The major mode, including process, environment and text-scale info."
    (concat (format "%s" mode-name)
            (if (stringp mode-line-process) mode-line-process)
            (if narf--env-version (concat " " narf--env-version))
            (and (featurep 'face-remap)
                 (/= text-scale-mode-amount 0)
                 (format " (%+d)" text-scale-mode-amount)))
    :tight-right t)

  (spaceline-define-segment *selection-info
    "Information about the current selection."
    (when (and active (evil-visual-state-p))
      (powerline-raw
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
       highlight-face)))

  ;; flycheck
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
    (when (and (bound-and-true-p flycheck-mode)
               (or flycheck-current-errors
                   (eq 'running flycheck-last-status-change)))
      (or (and (or (eq narf--flycheck-err-cache narf--flycheck-cache)
                   (memq flycheck-last-status-change '(running not-checked)))
               narf--flycheck-cache)
          (and (setq narf--flycheck-err-cache flycheck-current-errors)
               (setq narf--flycheck-cache
                     (let ((fe (narf/-flycheck-count 'error))
                           (fw (narf/-flycheck-count 'warning)))
                       (concat
                        (if fe (propertize (format " ⚠%s " fe)
                                           'face (if active
                                                     'spaceline-flycheck-error
                                                   'mode-line)))
                        (if fw (propertize (format " ⚠%s " fw)
                                           'face (if active
                                                     'spaceline-flycheck-warning
                                                   'mode-line)))))))))
    :tight t)

  (defvar narf--mode-line-padding
    (pl/percent-xpm powerline-height 100 0 100 0 1 nil nil))
  (spaceline-define-segment *pad
    "Padding, to ensure the mode-line is `powerline-height' pixels tall"
    narf--mode-line-padding
    :tight t)

  (spaceline-compile
   'main
   ;; Left side
   '(((*macro-recording *anzu *iedit *evil-substitute *flycheck)
      :skip-alternate t
      :tight t)
     *buffer-path
     *buffer-modified
     *vc)
   ;; Right side
   '(*selection-info
     *major-mode
     *env-version
     *buffer-encoding-abbrev
     (global :when active)
     *buffer-position
     *pad
     ))

  ;; Initialize modeline
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(provide 'core-ui)
;;; core-ui.el ends here
