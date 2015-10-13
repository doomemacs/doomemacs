;;; core-ui.el --- interface settings
;; see lib/ui-defuns.el

(when window-system
  (fringe-mode '(2 . 3))
  (set-frame-font narf-default-font)
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Highlight matching parens
(setq show-paren-delay 0.075)
(show-paren-mode 1)

(global-hl-line-mode   1)    ; do highlight line
(blink-cursor-mode     1)    ; do blink cursor
(tooltip-mode         -1)    ; don't show tooltips
(size-indication-mode -1)
;; Let spaceline handle these
(line-number-mode     -1)
(column-number-mode   -1)

(setq-default
 blink-matching-paren nil
 line-spacing 1
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
 fringes-outside-margins         t)     ; fringes on the other side of line numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package popwin
  :config
  (mapc (lambda (rule) (push rule popwin:special-display-config))
        '(("*quickrun*" :position bottom :height 15)
          ("*scratch*" :position bottom :height 20 :stick t :dedicated t)
          ("*helm-ag-edit*" :position bottom :height 20 :stick t)
          ("^\\*[Hh]elm.*?\\*\\'" :regexp t :position bottom :height 15)
          ("*eshell*" :position left :width 80 :stick t :dedicated t)
          ("*Apropos*" :position bottom :height 40 :stick t :dedicated t)
          ("*Backtrace*" :position bottom :height 15 :stick t)))
  (popwin-mode 1))

(use-package yascroll
  :config
  (add-hook! evil-insert-state-exit 'yascroll:update-scroll-bar)

  (defun yascroll:show-scroll-bar ()
    "Show scroll bar in BUFFER."
    (interactive)
    (yascroll:hide-scroll-bar)
    (let ((window-lines (window-height))
          (buffer-lines (count-lines (point-min) (point-max))))
      (when (< window-lines buffer-lines)
        (let* ((scroll-top (count-lines (point-min) (window-start)))
               (thumb-window-line (yascroll:compute-thumb-window-line window-lines buffer-lines scroll-top))
               (thumb-buffer-line (+ scroll-top thumb-window-line))
               (thumb-size (yascroll:compute-thumb-size window-lines buffer-lines))
               (make-thumb-overlay 'yascroll:make-thumb-overlay-right-fringe))
          (when (<= thumb-buffer-line buffer-lines)
            (yascroll:make-thumb-overlays make-thumb-overlay
                                          thumb-window-line
                                          thumb-size))))))

  (defun yascroll:compute-thumb-size (window-lines buffer-lines)
    "Return the proper size (height) of scroll bar thumb."
    (let ((window-lines (* window-lines 0.93)))
      (if (zerop buffer-lines)
          1
        (max 1 (floor (* (/ window-lines buffer-lines) window-lines))))))

  (setq yascroll:scroll-bar 'right-fringe
        yascroll:delay-to-hide nil)
  (add-to-list 'yascroll:enabled-window-systems 'mac)
  (defun yascroll:before-change (beg end))
  (global-yascroll-bar-mode 1))

(use-package fill-column-indicator
  :commands fci-mode
  :init
  (setq-default fill-column 80)
  (add-hook! (markdown-mode org-mode) 'fci-mode)
  :config
  (setq fci-rule-color "#2b303f"))

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
  (add-hook! (text-mode prog-mode scss-mode web-mode) 'narf|nlinum-enable)
  (add-hook! org-mode 'narf|nlinum-disable)
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
   powerline-default-separator 'wave
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
      (concat (if buffer-file-name
                  (concat (propertize
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
                               path))
                           'face (if (powerline-selected-window-active)
                                     'mode-line-buffer-dir
                                   'mode-line-inactive)
                           )
                          buffer
                          (if (and buffer-file-name (buffer-modified-p))
                              (propertize "%+" 'face 'mode-line-is-modified)))
                buffer)
              " "))
    ;; Causes right side of this segment to be square
    :face line-face
    :tight-right t)

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
    :when (not (string-match-p "unix" (symbol-name buffer-file-coding-system))))

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
    :face other-face
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

  (spaceline-define-segment narf-line-column
  "The current line and column numbers."
  "%l/%c")

  (spaceline-define-segment narf-evil-state
    "The current evil state.  Requires `evil-mode' to be enabled."
    (concat (substring (evil-state-property evil-state :tag t) 2 3) " ")
  :when (and active (bound-and-true-p evil-local-mode))
  :tight-right t)

  ;; Initialize modeline
  (spaceline-install
   ;; Left side
   '((narf-buffer-path remote-host)
     narf-vc
     narf-buffer-project-name
     ((flycheck-error flycheck-warning flycheck-info) :when active))
   ;; Right side
   '(selection-info
     anzu
     narf-env-version
     narf-buffer-encoding-abbrev
     ((" " :tight t)
      major-mode (minor-modes :separator " ")
      process :when active)
     (global :when active)
     (narf-line-column narf-buffer-position)
     )))

(provide 'core-ui)
;;; core-ui.el ends here
