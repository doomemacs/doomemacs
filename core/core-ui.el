;;; core-ui.el --- interface settings

(setq-default
 blink-matching-paren nil
 show-paren-delay 0.075

 ;; Multiple cursors across buffers cause a strange redraw delay for
 ;; some things, like auto-complete or evil-mode's cursor color
 ;; switching.
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows  nil
 hl-line-sticky-flag            nil  ; only highlight in one window

 uniquify-buffer-name-style     nil  ; my mode-line does this for me
 visible-bell                   nil  ; silence of the bells
 use-dialog-box                 nil  ; always avoid GUI
 redisplay-dont-pause           t
 indicate-buffer-boundaries     t
 indicate-empty-lines           t
 fringes-outside-margins        t
 idle-update-delay              2    ; update a little less often
 split-width-threshold          nil  ; favor horizontal splits
 show-help-function             nil  ; hide :help-echo text

 ;; Disable bidirectional text support for slight performance bonus
 bidi-display-reordering        nil

 ;; Minibuffer resizing
 resize-mini-windows            'grow-only
 max-mini-window-height         0.3

 ;; Remove arrow on the right fringe when wrapped
 fringe-indicator-alist (delq (assoc 'continuation fringe-indicator-alist)
                              fringe-indicator-alist))

(blink-cursor-mode  1)    ; blink cursor
(tooltip-mode      -1)    ; show tooltips in echo area

;; Set up minibuffer and fringe
(defface narf-minibuffer-active '((t (:inherit mode-line)))
  "Face for active minibuffer")

(if (not window-system)
    (menu-bar-mode -1)
  ;; Set fonts
  (narf/load-font narf-default-font)
  (set-face-attribute 'default t :font narf-default-font)

  ;; Setup fringe
  (fringe-mode narf-fringe-size)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-window-fringes (minibuffer-window) 0 0 nil)
  ;; Tilde empty-line indicator
  (define-fringe-bitmap 'tilde [64 168 16] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
  (set-fringe-bitmap-face 'tilde 'font-lock-comment-face)

  ;; Brighter minibuffer when active
  (defun narf|minibuffer-setup ()
    (set-window-fringes (selected-window) 0 0 nil)
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist '(default narf-minibuffer-active)))
  (add-hook! minibuffer-setup 'narf|minibuffer-setup))

;; Fix display of certain unicode characters
(mapc (lambda (set)
        (let ((font (car set))
              (chars  (cadr set))
              (size  (caddr set)))
          (mapc (lambda (x) (set-fontset-font
                        "fontset-default" `(,x . ,x)
                        (font-spec :name font :size size) nil 'prepend))
                chars)))
      '(("DejaVu Sans" (?☑ ?☐ ?✍ ?⚠ ?★ ?λ
                        ?➊ ?➋ ?➌ ?➍ ?➎ ?❻ ?➐ ?➑ ?➒ ?➓))
        ;; File attachment symbols (for org-mode)
        ("FontAwesome" (? ? ? ? ? ? ? ? ?) 13)
        ;; Math symbols
        ("Hack"        (?× ?∙ ?÷ ?⌉ ?⌈ ?⌊ ?⌋
                        ?∩ ?∪ ?⊆ ?⊂ ?⊄ ?⊇ ?⊃ ?⊅
                        ?⇒ ?⇐ ?⇔ ?↔ ?→ ?≡ ?∴ ?∵ ?⊕ ?∀ ?∃ ?∄ ?∈ ?∉
                        ?∨ ?∧ ?¬))))

;; on by default in Emacs 25
(when (and (featurep 'eldoc) (>= emacs-major-version 25))
  (global-eldoc-mode -1))

;; Highlight line
(add-hook! (prog-mode markdown-mode) 'hl-line-mode)

;; Disable line highlight in visual mode
(defvar narf--hl-line-mode nil)
(make-variable-buffer-local 'narf--hl-line-mode)

(defun narf|hl-line-on ()  (if narf--hl-line-mode (hl-line-mode +1)))
(defun narf|hl-line-off () (if narf--hl-line-mode (hl-line-mode -1)))

(add-hook! hl-line-mode (if hl-line-mode (setq narf--hl-line-mode t)))
(add-hook! evil-visual-state-entry 'narf|hl-line-off)
(add-hook! evil-visual-state-exit  'narf|hl-line-on)

;; Hide modeline in help windows
(add-hook! help-mode (setq-local mode-line-format nil))

;; Highlight TODO/FIXME/NOTE tags
(defface narf-todo-face  '((t (:inherit font-lock-warning-face))) "Face for TODOs")
(defface narf-fixme-face '((t (:inherit font-lock-warning-face))) "Face for FIXMEs")
(defface narf-note-face  '((t (:inherit font-lock-warning-face))) "Face for NOTEs")
(add-hook! (prog-mode emacs-lisp-mode)
  (font-lock-add-keywords nil '(("\\<\\(TODO\\((.+)\\)?:?\\)"  1 'narf-todo-face prepend)
                                ("\\<\\(FIXME\\((.+)\\)?:?\\)" 1 'narf-fixme-face prepend)
                                ("\\<\\(NOTE\\((.+)\\)?:?\\)"  1 'narf-note-face prepend))))

;; Fade out when unfocused
(add-hook! focus-in  (set-frame-parameter nil 'alpha 100))
(add-hook! focus-out (set-frame-parameter nil 'alpha 75))


;;
;; Plugins
;;

(use-package visual-fill-column :defer t)

(use-package yascroll
  :commands (yascroll-bar-mode)
  :config
  (add-to-list 'yascroll:enabled-window-systems 'mac)
  (setq yascroll:scroll-bar 'left-fringe
        yascroll:delay-to-hide nil))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook! (emacs-lisp-mode lisp-mode js2-mode scss-mode) 'rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 4))

(use-package rainbow-mode :defer t
  :init
  (add-hook! rainbow-mode
    (when narf--hl-line-mode
      (hl-line-mode (if rainbow-mode -1 1)))))

(use-package volatile-highlights
  :config
  (vhl/define-extension 'my-undo-tree-highlights
    'undo-tree-undo 'undo-tree-redo)
  (vhl/install-extension 'my-undo-tree-highlights)
  (vhl/define-extension 'my-yank-highlights
    'evil-yank 'evil-paste-after 'evil-paste-before 'evil-paste-pop)
  (vhl/install-extension 'my-yank-highlights)
  (volatile-highlights-mode t))

(use-package nlinum
  :commands nlinum-mode
  :preface
  (defvar narf--hl-nlinum-overlay nil)
  (defvar narf--hl-nlinum-line nil)
  (defvar nlinum-format "%4d  ")
  (defface linum-highlight-face '((t (:inherit linum))) "Face for line highlights")
  (setq linum-format "%3d ")
  :init
  (defun narf|nlinum-enable ()
    (nlinum-mode +1)
    (add-hook 'post-command-hook 'narf|nlinum-hl-line t))

  (defun narf|nlinum-disable ()
    (nlinum-mode -1)
    (remove-hook 'post-command-hook 'narf|nlinum-hl-line)
    (narf|nlinum-unhl-line))

  (add-hook!
    (markdown-mode prog-mode scss-mode web-mode conf-mode)
    'narf|nlinum-enable)
  :config
  (defun narf|nlinum-unhl-line ()
    "Unhighlight line number"
    (when narf--hl-nlinum-overlay
      (let* ((disp (get-text-property
                    0 'display (overlay-get narf--hl-nlinum-overlay 'before-string)))
             (str (nth 1 disp)))
        (put-text-property 0 (length str) 'face 'linum str)
        (setq narf--hl-nlinum-overlay nil
              narf--hl-nlinum-line nil)
        disp)))

  (defun narf|nlinum-hl-line (&optional line)
    "Highlight line number"
    (let ((line-no (or line (string-to-number (format-mode-line "%l")))))
      (when (and nlinum-mode (not (eq line-no narf--hl-nlinum-line)))
        (let* ((pbol (if line
                         (save-excursion (goto-char (point-min))
                                         (forward-line line-no)
                                         (line-beginning-position))
                       (line-beginning-position)))
               (peol (1+ pbol)))
          ;; Handle EOF case
          (let ((max (point-max)))
            (when (>= peol max)
              (setq peol max)))
          (jit-lock-fontify-now pbol peol)
          (let ((ov (-first (lambda (item) (overlay-get item 'nlinum)) (overlays-in pbol peol))))
            (when ov
              (narf|nlinum-unhl-line)
              (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
                (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                (setq narf--hl-nlinum-overlay ov
                      narf--hl-nlinum-line line-no))))))))

  (add-hook! nlinum-mode
    (setq nlinum--width
          (length (save-excursion (goto-char (point-max))
                                  (format-mode-line "%l"))))))


;; Mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package spaceline
  :init
  (defvar narf--env-version nil)
  (defvar narf--env-command nil)
  (make-variable-buffer-local 'narf--env-version)
  (make-variable-buffer-local 'narf--env-command)
  :config
  (setq-default
   powerline-default-separator nil
   powerline-height 19
   spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (defface mode-line-is-modified nil "Face for mode-line modified symbol")
  (defface mode-line-buffer-file nil "Face for mode-line buffer file path")

  (progn ;; Custom modeline segments
    (spaceline-define-segment *buffer-path
      (if buffer-file-name
          (let* ((project-path (let (projectile-require-project-root) (projectile-project-root)))
                 (buffer-path (f-relative buffer-file-name project-path))
                 (max-length (truncate (/ (window-width) 1.75)))
                 (path-len (length buffer-path)))
            (concat (file-name-nondirectory (directory-file-name project-path))
                    "/"
                    (if (> path-len max-length)
                        (concat "…" (replace-regexp-in-string
                                     "^.*?/" "/"
                                     (substring buffer-path (- path-len max-length) path-len)))
                      buffer-path)))
        "%b")
      :face (if active 'mode-line-buffer-file 'mode-line-inactive)
      :skip-alternate t
      :tight-right t)

    (spaceline-define-segment *remote-host
      "Hostname for remote buffers."
      (concat "@" (file-remote-p default-directory 'host))
      :when (file-remote-p default-directory 'host))

    (spaceline-define-segment *buffer-modified
      (concat
       (when buffer-file-name
         (concat
          (when (buffer-modified-p) "[+]")
          (unless (file-exists-p buffer-file-name) "[!]")))
       (if buffer-read-only "[RO]"))
      :face mode-line-is-modified
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
            (cond ((eq start 1) ":Top")
                  ((>= perc 100) ":Bot")
                  (t (format ":%d%%%%" perc))))))
      :tight t)

    (spaceline-define-segment *vc
      "Version control info"
      (powerline-raw
       (concat (replace-regexp-in-string
                (format "^ %s" (vc-backend buffer-file-name))
                "" vc-mode)
               (when buffer-file-name
                 (pcase (vc-state (buffer-file-name))
                   (`up-to-date "")
                   (`edited "*")
                   (`added "+")
                   (`unregistered "?")
                   (`removed "-")
                   (`needs-merge "%")
                   (`needs-update "^")
                   (`ignored "#")
                   (_ "_")))))
      :when (and active vc-mode)
      :face other-face
      :tight-right t)

    (spaceline-define-segment *env-version
      "Shows the environment version of a mode (e.g. pyenv for python or rbenv for ruby).
See `define-env-command!' to define one for a mode."
      narf--env-version
      :when narf--env-version
      :face other-face
      :skip-alternate t
      :tight-right t)


    (progn ;; search indicators
      (defface mode-line-count-face nil "")
      (make-variable-buffer-local 'anzu--state)
      (spaceline-define-segment *anzu
        "Show the current match number and the total number of matches. Requires
anzu to be enabled."
        (let ((here anzu--current-position)
              (total anzu--total-matched))
          (format " %s/%d%s "
                  (anzu--format-here-position here total)
                  total (if anzu--overflow-p "+" "")))
        :face (if active 'mode-line-count-face 'mode-line-inactive)
        :when (and (> anzu--total-matched 0) (evil-ex-hl-active-p 'evil-ex-search))
        :skip-alternate t
        :tight t)

      ;; TODO mode-line-iedit-face default face
      (spaceline-define-segment *iedit
        "Show the number of matches and what match you're on (or after). Requires
iedit."
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
        :face (if active 'mode-line-count-face 'mode-line-inactive)
        :skip-alternate t)

      (defface mode-line-substitute-face nil "")
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
        :face (if active 'mode-line-count-face 'mode-line-inactive)
        :skip-alternate t))

    (spaceline-define-segment *macro-recording
      "Show when recording macro"
      (format "%s ▶" (char-to-string evil-this-macro))
      :when (and active defining-kbd-macro)
      :face highlight-face
      :skip-alternate t)

    (spaceline-define-segment *buffer-encoding-abbrev
      "The line ending convention used in the buffer."
      (symbol-name buffer-file-coding-system)
      :when (not (string-match-p "\\(utf-8\\|undecided\\)"
                                 (symbol-name buffer-file-coding-system))))

    (spaceline-define-segment *major-mode
      (powerline-raw
       (concat
        (and (featurep 'face-remap) (/= text-scale-mode-amount 0) (format "(%+d) " text-scale-mode-amount))
        mode-name
        (if (stringp mode-line-process) mode-line-process)))
      :tight-right t)

    (spaceline-define-segment *buffer-size
      (powerline-buffer-size)
      :tight-right t
      :skip-alternate t)

    (defun narf--col-at-pos (pos)
      (save-excursion (goto-char pos) (current-column)))
    (spaceline-define-segment *selection-info
      "Information about the size of the current selection, when applicable.
Supports both Emacs and Evil cursor conventions."
      (let ((reg-beg (region-beginning))
            (reg-end (region-end)))
        (let* ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
               (chars (- (1+ reg-end) reg-beg))
               (cols (1+ (abs (- (narf--col-at-pos reg-end)
                                 (narf--col-at-pos reg-beg)))))
               (evil (eq 'visual evil-state))
               (rect (or (bound-and-true-p rectangle-mark-mode)
                         (and evil (eq 'block evil-visual-selection))))
               (multi-line (or (> lines 1) (eq 'line evil-visual-selection))))
          (cond
           (rect (format "%dx%dB" lines (if evil cols (1- cols))))
           (multi-line
            (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
                (format "%dL" lines)
              (format "%dC %dL" chars lines)))
           (t (format "%dC" (if evil chars (1- chars)))))))
      :when (eq 'visual evil-state)
      :face highlight-face
      :skip-alternate t)

    (progn ;; flycheck
      (defun narf--flycheck-count (state)
        "Return flycheck information for the given error type STATE."
        (let* ((counts (flycheck-count-errors flycheck-current-errors))
               (errorp (flycheck-has-current-errors-p state))
               (running (eq 'running flycheck-last-status-change))
               (err (cdr (assq state counts))))
          (when errorp (if running "?" err))))

      (defface spaceline-flycheck-error
        '((t (:foreground "#FC5C94" :distant-foreground "#A20C41")))
        "Face for flycheck error feedback in the modeline.")
      (defface spaceline-flycheck-warning
        '((t (:foreground "#F3EA98" :distant-foreground "#968B26")))
        "Face for flycheck warning feedback in the modeline.")
      (defface spaceline-flycheck-info
        '((t (:foreground "#8DE6F7" :distant-foreground "#21889B")))
        "Face for flycheck info feedback in the modeline.")

      (defvar narf--flycheck-err-cache nil "")
      (defvar narf--flycheck-cache nil "")
      (spaceline-define-segment *flycheck
        "Persistent and cached flycheck indicators in the mode-line."
        (or (and (or (eq narf--flycheck-err-cache narf--flycheck-cache)
                     (memq flycheck-last-status-change '(running not-checked)))
                 narf--flycheck-cache)
            (and (setq narf--flycheck-err-cache flycheck-current-errors)
                 (setq narf--flycheck-cache
                       (let ((fe (narf--flycheck-count 'error))
                             (fw (narf--flycheck-count 'warning))
                             (fi (narf--flycheck-count 'info)))
                         (concat
                          (when fe (powerline-raw (format " ⚠%s " fe) 'spaceline-flycheck-error))
                          (when fw (powerline-raw (format " ⚠%s " fw) 'spaceline-flycheck-warning))
                          (when fi (powerline-raw (format " ⚠%s " fi) 'spaceline-flycheck-info)))))))
        :when (and (bound-and-true-p flycheck-mode)
                   (or flycheck-current-errors
                       (eq 'running flycheck-last-status-change)))
        :tight t
        :skip-alternate t))

    (spaceline-define-segment *hud
      "A HUD that shows which part of the buffer is currently visible."
      (powerline-hud (if active 'spaceline-highlight-face 'region) line-face 1)
      :tight-right t))

  ;; Initialize modeline
  (spaceline-install
   ;; Left side
   '(*macro-recording
     ((*anzu *iedit *evil-substitute *flycheck)
      :fallback *buffer-size)
     (*buffer-path *remote-host)
     *buffer-modified
     *vc
     )
   ;; Right side
   '(*selection-info
     *buffer-encoding-abbrev
     *major-mode
     *env-version
     (global :when active)
     ("%l/%c" *buffer-position)
     *hud
     )))

(provide 'core-ui)
;;; core-ui.el ends here
