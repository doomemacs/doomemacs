;;; core-os-osx.el --- Mac-specific settings

(eval-when-compile (require 'core))

;; Use a shared clipboard
(setq x-select-enable-clipboard t
      select-enable-clipboard t
      ;; Prefixes: Command = M, Alt = A
      mac-command-modifier 'meta
      mac-option-modifier  'alt

      ;; sane trackpad/mouse scroll settings
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))  ;; one line at a time
      mouse-wheel-progressive-speed nil             ;; don't accelerate scrolling

      ;;; NOTE Meaningless to railwaycat's emacs-mac build
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ns-use-native-fullscreen nil
      ;; Don't open files from the workspace in a new frame
      ns-pop-up-frames nil)

;; fix emacs PATH on OSX (GUI only)
(when window-system
  (setenv "SHELL" "/usr/local/bin/zsh")
  ;; `exec-path-from-shell' is slow, so bring out the cache
  (setq exec-path
        (or (persistent-soft-fetch 'exec-path-env "osx")
            (progn
              (require 'exec-path-from-shell)
              (exec-path-from-shell-initialize)
              (persistent-soft-store 'exec-path-env exec-path "osx")
              exec-path))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narf-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (f-full (s-replace "'" "\\'"
                                  (or path (if (eq major-mode 'dired-mode)
                                               (dired-get-file-for-visit)
                                             (buffer-file-name))))))
         (command (format "open %s"
                          (if app-name
                              (format "-a %s '%s'" (shell-quote-argument app-name) path)
                            (format "'%s'" path)))))
    (message "Running: %s" command)
    (shell-command command)))

(defmacro open-with! (id &optional app dir)
  `(defun ,(intern (format "os-%s" id)) ()
     (interactive)
     (narf-open-with ,app ,dir)))

(open-with! open-in-default-program)
(open-with! open-in-browser "Google Chrome")
(open-with! reveal "Finder" default-directory)
(open-with! reveal-project "Finder" (narf/project-root))
(open-with! upload "Transmit")
(open-with! upload-folder "Transmit" default-directory)
(open-with! send-to-launchbar "LaunchBar")
(open-with! send-project-to-launchbar "LaunchBar" (narf/project-root))

(defun os-switch-to-term ()
  (interactive)
  (do-applescript "tell application \"iTerm\" to activate"))

(defun os-switch-to-term-and-cd ()
  (interactive)
  (narf:send-to-tmux (format "cd %s" (shell-quote-argument default-directory)))
  (narf-switch-to-iterm))

(defun narf-org-init-for-osx ()
  ;; Reveal files in finder
  (defvar org-file-apps '(("\\.org$" . emacs)
                          (t . "open `dirname \"%s\"`"))))

(provide 'core-os-osx)
;;; core-os-osx.el ends here
