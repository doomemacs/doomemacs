;;; core-ui.el --- interface settings
;; see lib/ui-defuns.el

(when window-system
  (fringe-mode '(1 . 8))
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq show-paren-delay 0)

(global-hl-line-mode  1)    ; do highlight line
(blink-cursor-mode    1)    ; do blink cursor
(line-number-mode     1)    ; do show line no in modeline
(column-number-mode   1)    ; do show col no in modeline
(tooltip-mode        -1)    ; don't show tooltips
;; (size-indication-mode 1)    ; do show file size

(setq-default
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

(use-package nlinum ; line numbers
  :disabled t
  :defer t
  :defines nlinum--width
  :preface
  (defface linum '((t (:inherit default)))
    "Face for line numbers" :group 'nlinum-mode)
  (defface linum-highlight-face '((t (:inherit linum)))
    "Face for line highlights" :group 'nlinum-mode)
  (defvar narf--hl-nlinum-overlay nil)
  (defvar narf--hl-nlinum-line    nil)
  (defvar nlinum-format " %3d ")
  :init
  ;; Highlight line number
  (defun narf|nlinum-unhl-line ()
    (when narf--hl-nlinum-overlay
      (let* ((ov narf--hl-nlinum-overlay)
             (disp (get-text-property 0 'display (overlay-get ov 'before-string)))
             (str (nth 1 disp)))
        (put-text-property 0 (length str) 'face 'linum str)
        (setq narf--hl-nlinum-overlay nil
              narf--hl-nlinum-line nil))))

  (defun narf|nlinum-hl-line (&optional line)
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

  (defun narf|nlinum-enable ()
    (nlinum-mode +1)
    (add-hook 'post-command-hook 'narf|nlinum-hl-line))

  (defun narf|nlinum-disable ()
    (nlinum-mode -1)
    (remove-hook 'post-command-hook 'narf|nlinum-hl-line)
    (narf|nlinum-unhl-line))

  ;; Preset width nlinum
  (add-hook! (text-mode prog-mode scss-mode web-mode) 'narf|nlinum-enable)
  (add-hook! org-mode 'narf|nlinum-disable)
  (add-hook! nlinum-mode
    (setq nlinum--width (length (number-to-string (count-lines (point-min) (point-max)))))))

;; Mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package spaceline-segments
  :init
  (setq-default
   powerline-default-separator 'wave
   powerline-height 18)
  :config
  (require 'spaceline-segments)

  (spaceline-define-segment narf-buffer-path
    "Name of buffer."
    (concat (let ((buffer-path (buffer-file-name)))
              (if (and buffer-path (file-exists-p buffer-path))
                  (progn
                    (setq buffer-path (abbreviate-file-name buffer-path))
                    (let ((path (file-relative-name buffer-path (file-name-directory (narf/project-root))))
                          (max-length (/ (window-width) 2)))
                      (if (> (length path) max-length)
                          (concat "…" (replace-regexp-in-string
                                       "^.*?/" "/"
                                       (let ((l (length path))) (substring path (- l max-length) l))))
                        path)))
                (powerline-buffer-id)))
            (if (buffer-modified-p)
                (propertize "*" 'font-lock-face `(:inherit ,other-face :foreground "orange")))
            " ")
    :tight-right t)

  (spaceline-define-segment narf-buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (let ((buf-coding (format "%s" buffer-file-coding-system)))
      (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
          (match-string 1 buf-coding)
        buf-coding))
    :when (not (string-match "unix" (symbol-name buffer-file-coding-system))))

  (spaceline-define-segment narf-line-column
    "The current line and column numbers."
    "%l/%c")

  (spaceline-define-segment narf-buffer-position
    "A more vim-like buffer position."
    (let ((perc (/ (window-end) 0.01 (point-max))))
      (cond ((eq (window-start) 1) ":Top")
            ((>= perc 100) ":Bot")
            (t (format ":%d%%%%" perc)))))

  (spaceline-define-segment narf-vc
    "Version control info"
    (replace-regexp-in-string (regexp-quote (symbol-name (vc-deduce-backend))) "" (s-trim (powerline-vc)) t t)
    :when (powerline-vc))

  (spaceline-define-segment narf-hud
    "A HUD that shows which part of the buffer is currently visible."
    (powerline-hud highlight-face default-face)
    :tight t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (spaceline-install
   ;; Left side
   '(((narf-buffer-path :face other-face) remote-host)
     ((flycheck-error flycheck-warning flycheck-info)
      :when active)
     (narf-vc :face other-face :when active))
   ;; Right side
   '(selection-info
     narf-buffer-encoding-abbrev
     (major-mode (minor-modes process :when active))
     (global :when active)
     (narf-line-column narf-buffer-position :face powerline-border)
     narf-hud)))

(provide 'core-ui)
;;; core-ui.el ends here
