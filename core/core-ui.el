;;; core-ui.el --- interface settings
;; see lib/ui-defuns.el

(when window-system
  (fringe-mode '(2 . 8))
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq show-paren-delay 0)

(global-hl-line-mode  1)    ; do highlight line
(blink-cursor-mode    1)    ; do blink cursor
(line-number-mode     1)    ; do show line no in modeline
(column-number-mode   1)    ; do show col no in modeline
(size-indication-mode 1)    ; do show file size
(tooltip-mode        -1)    ; don't show tooltips

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

(setq-default powerline-default-separator nil)
(require 'spaceline-segments)
(spaceline-install
 ;; Left side
 '((buffer-size)
   (buffer-id remote-host buffer-modified)
   ((flycheck-error flycheck-warning flycheck-info)
    :when active)
   (version-control :when active))
 ;; Right side
 `((battery :when active)
   selection-info
   major-mode
   (((minor-modes :separator spaceline-minor-modes-separator)
     process)
    :when active)
   (buffer-encoding-abbrev
    point-position
    line-column)
   (global :when active)
   buffer-position
   hud))

(provide 'core-ui)
;;; core-ui.el ends here
