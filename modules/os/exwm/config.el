;;; os/exwm/config.el -*- lexical-binding: t; -*-

;; Use the `ido' configuration for a few configuration fixes that alter
;; 'C-x b' workplace switching behaviour
(require 'exwm)
(require 'exwm-config)
(exwm-config-ido)
(ido-mode +1)

;; Configure `exwm-randr' to support multi-monitor setups as well as
;; hot-plugging HDMI outputs. Read more at
;; https://github.com/ch11ng/exwm/wiki#randr-multi-screen
(require 'exwm-randr)
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
(exwm-randr-enable)

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
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Here we configure some setup to run after `exwm' has loaded. Any configuration
;; that is not critical before startup (such as monitor monitor configuration) should
;; go here.
(after! exwm-mode
  ;; Here we configure our rudamentary status bar.
  (when (featurep! +status)
    (setq display-time-default-load-average nil)
    (display-time-mode +1)
    (display-battery-mode +1))

  ;; Here we configure `exwm-firefox-*'.
  (when (featurep! +firefox)
    ;; Here we add the <ESC> key to the exwm input keys for firefox buffers.
    (dolist (k `(escape))
      (cl-pushnew k exwm-input-prefix-keys))

    ;; Here we configure further depending if the user has evil mode enabled
    ;; in their modules.
    (if (featurep! :editor evil)
        (lambda ()
          ;; The user has `evil-mode'.
          (require 'exwm-firefox-evil)
          (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox))
      (lambda ()
        ;; The user does NOT have `evil-mode'.
        (require 'exwm-firefox-core)))))

;; Here we enable `exwm' to launch when everything is ready:
(exwm-enable)
