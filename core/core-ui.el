;;; core-ui.el --- interface settings
;; see lib/ui-defuns.el

(when window-system
  (fringe-mode '(1 . 8))
  (set-frame-font narf-default-font)
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Highlight matching parens
(setq show-paren-delay 0.05)
(show-paren-mode 1)

(global-hl-line-mode   1)    ; do highlight line
(blink-cursor-mode     1)    ; do blink cursor
(line-number-mode      1)    ; do show line no in modeline
(column-number-mode    1)    ; do show col no in modeline
(tooltip-mode         -1)    ; don't show tooltips
(size-indication-mode -1)

(setq-default
 line-spacing 1
 ;; Multiple cursors across buffers cause a strange redraw delay for
 ;; some things, like auto-complete or evil-mode's cursor color
 ;; switching.
 cursor-in-non-selected-windows  nil

 visible-bell                    nil    ; silence of the bells
 use-dialog-box                  nil    ; avoid GUI
 redisplay-dont-pause            t
 indicate-buffer-boundaries      nil
 indicate-empty-lines            nil
 fringes-outside-margins         t)     ; fringes on the other side of line numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fill-column-indicator
  :config
  (setq fci-rule-color "#2b303f")
  (setq-default fill-column 80)
  (add-hook! text-mode 'fci-mode))

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
  (defface mode-line-buffer-path nil "Face for mode-line buffer directory")

  ;; Custom modeline segments
  (spaceline-define-segment narf-buffer-path
    "Base filename of buffer."
    (if buffer-file-name
        (concat (file-name-nondirectory buffer-file-name)
                (if (buffer-modified-p)
                    (propertize "*" 'face 'mode-line-is-modified))
                " ")
      (powerline-buffer-id))
    :tight t)

  (spaceline-define-segment narf-buffer-dir
    "Buffer file directory."
    (propertize
     (or narf--spaceline-file-path
         (setq narf--spaceline-file-path
               (let* ((max-length (/ (window-width) 2))
                      (project-path (narf/project-root))
                      (path (file-name-directory
                             (file-relative-name buffer-file-name
                                                 (file-name-directory (if (string-match "/+\\'" project-path)
                                                                          (replace-match "" t t project-path)
                                                                        project-path)))))
                      (path-len (length path)))
                 (if (> path-len max-length)
                     (concat "…" (replace-regexp-in-string
                                  "^.*?/" "/"
                                  (substring path (- path-len max-length) path-len)))
                   path))))
     'face (if (powerline-selected-window-active)
               'mode-line-buffer-path
             'mode-line-inactive))
    :when buffer-file-name
    :tight-right t)

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
        (format "%s %s%s" (char-to-string #xe0a0) vc
                (case (vc-state buffer-file-name) ('edited "+") ('conflict "!!!") (t "")))))
    :when (and active vc-mode))

  (spaceline-define-segment narf-hud
    "A HUD that shows which part of the buffer is currently visible."
    (powerline-hud highlight-face default-face)
    :tight t)

  ;; Display version string
  (defvar narf--env-version nil)
  (defvar narf--env-command nil)
  (make-variable-buffer-local 'narf--env-version)
  (make-variable-buffer-local 'narf--env-command)

  (spaceline-define-segment narf-env-version
    "A HUD that shows which part of the buffer is currently visible."
    (unless narf--env-version
      (narf|spaceline-env-update))
    narf--env-version
    :when (and narf--env-version (memq major-mode '(ruby-mode enh-ruby-mode python-mode))))

  ;; Initialize modeline
  (spaceline-install
   ;; Left side
   '((narf-buffer-dir :face other-face)
     (narf-buffer-path remote-host)
     (narf-vc)
     ((flycheck-error flycheck-warning flycheck-info) :face other-face :when active))
   ;; Right side
   '(selection-info
     narf-env-version
     narf-buffer-encoding-abbrev
     (major-mode (minor-modes :separator " ") process :when active)
     (global :when active)
     narf-buffer-position
     narf-hud)))

(provide 'core-ui)
;;; core-ui.el ends here
