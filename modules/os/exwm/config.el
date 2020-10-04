;;; os/exwm/config.el -*- lexical-binding: t; -*-

;; Define custom variables for the `exwm-update-class-hook' for users to
;; configure which buffers names should NOT be modified.
(defvar exwm/ignore-wm-prefix "sun-awt-X11-"
  "Don't rename exwm buffers with this prefix.")
(defvar exwm/ignore-wm-name "gimp"
  "Don't rename exwm buffers with this name.")

;; Make sure `exwm' windows can be restored when switching workspaces.
(defun exwm--update-utf8-title-advice (oldfun id &optional force)
  "Only update the window title when the buffer is visible."
  (when (get-buffer-window (exwm--id->buffer id))
    (funcall oldfun id force)))

;; Confgure `exwm' the X window manager for emacs.
(use-package! exwm
  :config
  ;; Configure global key bindings.
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-worksplace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  ;; Configure the default buffer behaviour. All buffers created in `exwm-mode'
  ;; are named "*EXWM*". Change it in `exwm-update-class-hook' and `exwm-update-title-hook'
  ;; which are run when a new X window class name or title is available.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p exwm/ignore-wm-prefix exwm-instance-name)
                          (string= exwm/ignore-wm-name exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p exwm/ignore-wm-prefix exwm-instance-name)
                        (string= exwm/ignore-wm-name exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; Show `exwm' buffers in buffer switching prompts.
  (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Restore window configurations involving exwm buffers by only changing names
  ;; of visible buffers.
  (advice-add #'exwm--update-utf8-title :around #'exwm--update-utf8-title-advice)

  ;; Enable the window manager.
  (exwm-enable))

;; Use the `ido' configuration for a few configuration fixes that alter
;; 'C-x b' workplace switching behaviour. This also effects the functionality
;; of 'SPC .' file searching in doom regardless of the users `ido' configuration.
(use-package! exwm-config
  :after exwm
  :config
  (exwm-config--fix/ido-buffer-window-other-frame))

;; Configure `exwm-randr' to support multi-monitor setups as well as
;; hot-plugging HDMI outputs. Read more at:
;; https://github.com/ch11ng/exwm/wiki#randr-multi-screen
(use-package! exwm-randr
  :after exwm
  :config
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
                    default-output)
                (with-temp-buffer
                  (call-process "xrandr" nil t nil)
                  (goto-char (point-min))
                  (re-search-forward xrandr-output-regexp nil 'noerror)
                  (setq default-output (match-string 1))
                  (forward-line)
                  (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
                      (call-process
                       "xrandr" nil nil nil
                       "--output" default-output
                       "--auto")
                    (call-process
                     "xrandr" nil nil nil
                     "--output" (match-string 1) "--primary" "--auto"
                     "--output" default-output "--off")
                    (setq exwm-randr-workspace-monitor-plist
                          (list 0 (match-string 1))))))))
  (exwm-randr-enable))

;; Configure emacs input methods in all X windows.
(when (featurep! +xim)
  (use-package! exwm-xim
    :after exwm
    :config
    ;; These variables are required for X programs to pick up Emacs IM.
    (setenv "XMODIFIERS" "@im=exwm-xim")
    (setenv "GTK_IM_MODULE" "xim")
    (setenv "QT_IM_MODULE" "xim")
    (setenv "CLUTTER_IM_MODULE" "xim")
    (setenv "QT_QPA_PLATFORM" "xcb")
    (setenv "SDL_VIDEODRIVER" "x11")
    (exwm-xim-enable)))

;; Configure the rudamentary status bar.
(when (featurep! +status)
  (setq display-time-default-load-average nil)
  (display-time-mode +1)
  (display-battery-mode +1))

;; Configure `exwm-firefox-*'.
(when (featurep! +firefox)
  (use-package! exwm-firefox-core
    :after exwm
    :config
    ;; Add the <ESC> key to the exwm input keys for firefox buffers.
    (dolist (k `(escape))
      (cl-pushnew k exwm-input-prefix-keys)))

  ;; Configure further depending if the user has evil mode enabled.
  (when (featurep! :editor evil)
    (use-package! exwm-firefox-evil
      :after exwm
      :config
      ;; Add the firefox wm class name.
      (dolist (k `("firefox"))
        (cl-pushnew k exwm-firefox-evil-firefox-class-name))
      ;; Add the firefox buffer hook
      (add-hook 'exwm-manage-finish-hook
              'exwm-firefox-evil-activate-if-firefox))))
