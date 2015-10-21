;;; core-ui.el --- interface settings
;; see lib/ui-defuns.el

(when window-system
  (set-frame-font narf-default-font)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (fringe-mode '(2 . 3)))

;; Highlight matching parens
(setq show-paren-delay 0.075)

(global-hl-line-mode   1)    ; do highlight line
(blink-cursor-mode     1)    ; do blink cursor
(tooltip-mode         -1)    ; don't show tooltips
(size-indication-mode -1)
;; Let spaceline handle these
(line-number-mode     -1)
(column-number-mode   -1)

(setq-default
 blink-matching-paren nil
 ;; Multiple cursors across buffers cause a strange redraw delay for
 ;; some things, like auto-complete or evil-mode's cursor color
 ;; switching.
 cursor-in-non-selected-windows  nil

 uniquify-buffer-name-style      nil

 visible-bell                    nil    ; silence of the bells
 use-dialog-box                  nil    ; avoid GUI
 redisplay-dont-pause            t
 indicate-buffer-boundaries      nil
 indicate-empty-lines            nil
 fringes-outside-margins         t      ; fringes on the other side of line numbers

 resize-mini-windows t)

;; hl-line-mode breaks minibuffer in TTY
(add-hook! minibuffer-setup
  (make-variable-buffer-local 'global-hl-line-mode)
  (setq global-hl-line-mode nil))

;; Hide modeline in help windows
(add-hook! help-mode (setq-local mode-line-format nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hl-todo
  :commands hl-todo-mode
  :init
  (add-hook! prog-mode 'hl-todo-mode)
  (defvar hl-todo-keyword-faces
    '(("TODO" . "#cc9393")
      ("NOTE" . "#d0bf8f")
      ("FIXME" . "#cc9393"))))

(use-package hideshow
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :diminish hs-minor-mode
  :init
  (after! evil
    (defun narf-load-hs-minor-mode ()
      (advice-remove 'evil-toggle-fold 'narf-load-hs-minor-mode)
      (hs-minor-mode 1))
    (advice-add 'evil-toggle-fold :before 'narf-load-hs-minor-mode)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :when (display-graphic-p)
  :init (add-hook! (emacs-lisp-mode js2-mode scss-mode) 'rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-outermost-only-face-count 1))

(use-package rainbow-mode :defer t)

(use-package popwin
  :config
  (setq popwin:popup-window-height 25)
  (mapc (lambda (rule) (push rule popwin:special-display-config))
        '(("*quickrun*" :position bottom :height 15)
          ("*scratch*" :position bottom :height 20 :stick t :dedicated t)
          ("*helm-ag-edit*" :position bottom :height 20 :stick t)
          (help-mode :position bottom :height 15 :stick t)
          ("^\\*[Hh]elm.*?\\*\\'" :regexp t :position bottom :height 15)
          ("*eshell*" :position left :width 80 :stick t :dedicated t)
          ("*Apropos*" :position bottom :height 40 :stick t :dedicated t)
          ("*Backtrace*" :position bottom :height 15 :stick t)))
  (popwin-mode 1))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (vhl/define-extension 'my-evil-highlights
    'evil-yank
    'evil-paste-pop-proxy
    'evil-paste-pop-next
    'evil-paste-after
    'evil-paste-before)
  (vhl/install-extension 'my-evil-highlights)

  (vhl/define-extension 'my-undo-tree-highlights
    'undo-tree-undo
    'undo-tree-redo)
  (vhl/install-extension 'my-undo-tree-highlights)
  (volatile-highlights-mode t))

(use-package nlinum ; line numbers
  :preface
  (defvar narf--hl-nlinum-overlay nil)
  (defvar narf--hl-nlinum-line    nil)
  (defvar nlinum-format " %3d ")
  :init
  (defface linum-highlight-face '((t (:inherit linum))) "Face for line highlights")

  (defun narf|nlinum-enable ()
    (nlinum-mode +1)
    (add-hook! post-command 'narf|nlinum-hl-line))

  (defun narf|nlinum-disable ()
    (nlinum-mode -1)
    (remove-hook 'post-command-hook 'narf|nlinum-hl-line)
    (narf|nlinum-unhl-line))

  ;; Preset width nlinum
  (add-hook! (markdown-mode prog-mode scss-mode web-mode) 'narf|nlinum-enable)
  :config
  (defun narf|nlinum-unhl-line ()
    "Highlight line number"
    (when narf--hl-nlinum-overlay
      (let* ((ov narf--hl-nlinum-overlay)
             (disp (get-text-property 0 'display (overlay-get ov 'before-string)))
             (str (nth 1 disp)))
        (put-text-property 0 (length str) 'face 'linum str)
        (setq narf--hl-nlinum-overlay nil
              narf--hl-nlinum-line nil))))

  (defun narf|nlinum-hl-line (&optional line)
    "Unhighlight line number"
    (let ((line-no (or line (line-number-at-pos (point)))))
      (when (and nlinum-mode (not (eq line-no narf--hl-nlinum-line)))
        (let* ((pbol (if line (save-excursion (goto-char (point-min))
                                              (forward-line line-no)
                                              (point-at-bol))
                       (point-at-bol)))
               (peol (1+ pbol)))
          ;; Handle EOF case
          (when (>= peol (point-max))
            (setq peol (point-max)))
          (jit-lock-fontify-now pbol peol)
          (let* ((overlays (overlays-in pbol peol))
                 (ov (-first (lambda (item) (overlay-get item 'nlinum)) overlays)))
            (when ov
              (narf|nlinum-unhl-line)
              (let* ((disp (get-text-property 0 'display (overlay-get ov 'before-string)))
                     (str (nth 1 disp)))
                (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                (setq narf--hl-nlinum-overlay ov
                      narf--hl-nlinum-line line-no))))))))

  (add-hook! nlinum-mode
    (setq nlinum--width (length (number-to-string (count-lines (point-min) (point-max)))))))

;; Mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package spaceline-segments
  :config
  (setq-default
   powerline-default-separator nil
   powerline-height 18)

  ;; Modeline cache
  (defvar narf--spaceline-file-path nil)
  (make-variable-buffer-local 'narf--spaceline-file-path)
  (add-hook! focus-in (setq narf--spaceline-file-path nil))

  (defface mode-line-is-modified nil "Face for mode-line modified symbol")
  (defface mode-line-buffer-file nil "Face for mode-line buffer file path")
  (defface mode-line-buffer-dir nil "Face for mode-line buffer dirname")

  ;; Custom modeline segments
  (spaceline-define-segment narf-buffer-path
    "Buffer file path."
    (let ((buffer (propertize "%b" 'face
                              (if (powerline-selected-window-active)
                                  'mode-line-buffer-file
                                'mode-line-inactive))))
      (if buffer-file-name
          (concat (propertize
                   (or narf--spaceline-file-path
                       (setq narf--spaceline-file-path
                             (let* ((max-length (/ (window-width) 2))
                                    (project-path (let ((p (narf/project-root)))
                                                    (if (string-match "/+\\'" p)
                                                        (replace-match "" t t p)
                                                      p)))
                                    (path (f-dirname (f-relative buffer-file-truename (f-dirname project-path))))
                                    (path-len (length path)))
                               (if (> path-len max-length)
                                   (concat "…" (replace-regexp-in-string
                                                "^.*?/" "/"
                                                (substring path (- path-len max-length) path-len)))
                                 path))))
                   'face (if (powerline-selected-window-active)
                             'mode-line-buffer-dir
                           'mode-line-inactive))
                  buffer
                  (when buffer-file-name
                    (propertize
                     (concat
                      (when (buffer-modified-p) "[+]")
                      (when buffer-read-only "[RO]")
                      (unless (file-exists-p buffer-file-name) "[!]"))
                     'face (if active 'mode-line-is-modified 'mode-line-inactive))))
        buffer)))

  (spaceline-define-segment narf-buffer-project-name
    "The project name."
    (file-name-nondirectory (f-expand (narf/project-root)))
    :when (and (not (derived-mode-p 'special-mode))
               (string-match-p "^ ?\\*" (buffer-name))))

  (spaceline-define-segment narf-buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (let ((buf-coding (symbol-name buffer-file-coding-system)))
      (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
          (match-string 1 buf-coding)
        buf-coding))
    :when (and buffer-file-name
               (not (string-match-p "unix" (symbol-name buffer-file-coding-system)))))

  (spaceline-define-segment narf-buffer-position
    "A more vim-like buffer position."
    (let ((perc (/ (window-end) 0.01 (point-max))))
      (cond ((eq (window-start) 1) ":Top")
            ((>= perc 100) ":Bot")
            (t (format ":%d%%%%" perc)))))

  (spaceline-define-segment narf-vc
    "Version control info"
    (let ((vc (vc-working-revision buffer-file-name)))
      (when vc
        (format " %s %s%s " (char-to-string #xe0a0) vc
                (case (vc-state buffer-file-name) ('edited "+") ('conflict "!!!") (t "")))))
    :when (and active vc-mode)
    :tight t)

  ;; Display version string
  (defvar narf--env-version nil)
  (defvar narf--env-command nil)
  (make-variable-buffer-local 'narf--env-version)
  (make-variable-buffer-local 'narf--env-command)

  (spaceline-define-segment narf-env-version
    "A HUD that shows which part of the buffer is currently visible."
    (when (and narf--env-command (not narf--env-version))
      (narf|spaceline-env-update))
    narf--env-version
    :when (and narf--env-version (memq major-mode '(ruby-mode enh-ruby-mode python-mode))))

  (spaceline-define-segment narf-hud
    "A HUD that shows which part of the buffer is currently visible."
    (powerline-hud highlight-face default-face 1)
    :tight t)

  (spaceline-define-segment narf-anzu
    "Show the current match number and the total number of matches.  Requires anzu
to be enabled."
    (let ((here anzu--current-position)
          (total anzu--total-matched))
      (when anzu--state
        (cl-case anzu--state
          (search (format "%s/%d%s"
                          (anzu--format-here-position here total)
                          total (if anzu--overflow-p "+" "")))
          (replace-query (format "%d replace" total))
          (replace (format "%d/%d" here total)))))
    :when (and active (bound-and-true-p anzu--state)))

  ;; Initialize modeline
  (spaceline-install
   ;; Left side
   '((narf-anzu :face highlight-face)
     (narf-buffer-path remote-host)
     narf-vc
     narf-buffer-project-name
     ((flycheck-error flycheck-warning flycheck-info) :when active))
   ;; Right side
   '((selection-info :face highlight-face)
     narf-env-version
     narf-buffer-encoding-abbrev
     ((" " :tight t)
      major-mode (minor-modes :tight t :separator " ")
      process :when active)
     (global :when active)
     ("%l/%c" narf-buffer-position)
     narf-hud
     )))

(provide 'core-ui)
;;; core-ui.el ends here
