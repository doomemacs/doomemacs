;;; core-os-osx.el --- Mac-specific settings

(eval-when-compile (require 'core))

;; Use a shared clipboard
(setq x-select-enable-clipboard t
      ;; Prefixes: Command = M, Alt = A
      mac-command-modifier 'meta
      mac-option-modifier  'alt

      ;; sane trackpad/mouse scroll settings
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(8 ((shift) . 2))  ;; one line at a time
      mouse-wheel-progressive-speed nil             ;; don't accelerate scrolling


      ;;; NOTE These mean nothing to railwaycat's emacs-mac build on OSX
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ns-use-native-fullscreen nil
      ;; Don't open files from the workspace in a new frame
      ns-pop-up-frames nil)

;; fix emacs PATH on OSX (GUI only)
(when window-system
  (setenv "SHELL" "/usr/local/bin/zsh")
  (setenv "EMACS" "1") ; make sure the world knows
  (setq exec-path (eval-when-compile
                    (require 'exec-path-from-shell)
                    (exec-path-from-shell-initialize)
                    exec-path)))

;; OSX Related Plugins ;;;;;;;;;;;;;;;;;

(use-package applescript-mode :mode "\\.applescript$")

(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :config
  (mapc (lambda (r) (add-to-list 'dash-at-point-mode-alist r))
        `((java-mode . "java,droid,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc")
          (scss-mode . ,(cdr (assoc 'sass-mode dash-at-point-mode-alist)))
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! evil
  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
    (unless (or (featurep 'mac) (featurep 'ns)) ad-do-it)))

(defun narf-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (f-full (s-replace "'" "\\'" (or path (if (eq major-mode 'dired-mode) (dired-get-file-for-visit) (buffer-file-name))))))
         (command (concat "open " (when app-name (concat "-a " (shell-quote-argument app-name))) " '" path "'")))
    (message "Running: %s" command)
    (shell-command command)))

(defun narf-switch-to-iterm ()
  (interactive)
  (shell-command "osascript -e 'tell application \"iTerm2\" to activate'" nil))

(defun narf-switch-to-iterm-and-cd ()
  (interactive)
  (narf:tmux-chdir nil t)
  (shell-command "osascript -e 'tell application \"iTerm2\" to activate'" nil))

(provide 'core-os-osx)
;;; core-os-osx.el ends here
