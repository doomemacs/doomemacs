;;; os/exwm/config.el -*- lexical-binding: t; -*-

;; Here we configure `exwm' a fully functional X window manager:
;;
;; Use the `ido' configuration for a few configuration fixes that alter
;; 'C-x b' workplace switching behaviour
(require 'exwm)
(require 'exwm-config)
(exwm-config-ido)
(ido-mode +1)

;; Here we configure `exwm-randr' to support multi-monitor setups
;; as well as hot-plugging HDMI outputs:
(require 'exwm-randr)

;; Here we configure automated behaviour to only enable the
;; connected external screen, and to automatically revert to the
;; initial screen after disconnection:
;; https://github.com/ch11ng/exwm/wiki#randr-multi-screen
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

;; Here we enable a rudmentary status bar by displaying the time
;; and battery information (for systems with battery power) in the
;; modeline:
(if (featurep! +status)
    (and (setq display-time-default-load-average nil)
         (display-time-mode +1)
         (display-battery-mode +1)))

;; Here we configure global key bindings:
;;
;; - `s-r' to exit char / fullscreen mode
;; - `s-w' to switch workplaces interactively
;; - `s-0' to `s-9' to switch workplaces by index
;; - `s-&' to launch applications
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

;; Here we conigure `exwm-firefox-*' for the correct configuration based
;; on if the user has `evil-mode' activated in their configuration.
;;
;; - Add the <ESC> key to the exwm input keys for firefox buffers
;; - Add "firefox" to the list of firefox class names (evil users)
(if (featurep! +firefox)
    (and (dolist (k `(escape))
           (cl-pushnew k exwm-input-prefix-keys))
         (if (featurep! :editor evil)
             (and (require 'exwm-firefox-evil)
                  (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)
                  (dolist (k `("firefox"))
                    (cl-pushnew k exwm-firefox-evil-firefox-class-name)))))
  (and (require 'exwm-firefox)
       (exwm-firefox-mode +1)))

;; Here we configure the default buffer behaviour. All buffers created in
;; EXWM mode are named "*EXWM*". You may want to change it in
;; `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new X window class name or title is available:
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

;; Here we enable `exwm' to launch when everything is ready:
(exwm-enable)
