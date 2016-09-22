;;; core-os-osx.el --- Mac-specific settings

(global-set-key (kbd "M-q") 'kill-emacs)

(setq ;; Prefixes: Command = M, Alt = A
      mac-command-modifier 'meta
      mac-option-modifier  'alt
      ;; sane trackpad/mouse scroll settings
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ;; NOTE Meaningless to railwaycat's emacs-mac build
      ns-use-native-fullscreen nil
      ;; Don't open files from the workspace in a new frame
      ns-pop-up-frames nil)

;; fix emacs PATH on OSX (GUI only)
(when window-system
  (setenv "SHELL" "/usr/local/bin/zsh")
  ;; `exec-path-from-shell' is slow, so bring out the cache
  (setq exec-path
        (eval-when-compile
          (require 'exec-path-from-shell)
          (exec-path-from-shell-initialize)
          exec-path)))

;; Enable mouse support in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (λ! (scroll-down 4)))
  (global-set-key [mouse-5] (λ! (scroll-up 4)))

  (defun track-mouse (e))
  (setq mouse-sel-mode t))


;;
;; OSX-related plugins + hacks
;;

(use-package applescript-mode
  :mode "\\.applescript$"
  :init (add-hook 'applescript-mode-hook 'nlinum-mode)
  :config
  (def-docset! applescript-mode "applescript")
  (after! quickrundb
    (quickrun-add-command
     "applescript" `((:command . ,as-osascript-command)
                     (:cmdopt . "-ss %s")
                     (:description . "Run applescript"))
     :mode 'applescript-mode)))

(def-project-type! lb6 "lb6"
  :match "\\.lb\\(action\\|ext\\)/.+$"
  :build (lambda ()
           (awhen (f-traverse-upwards (lambda (f) (f-ext? f "lbaction")))
             (shell-command (format "open '%s'" it)))))

(after! evil
  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (when (or (featurep 'mac) (featurep 'ns))
    (advice-add 'evil-visual-update-x-selection :override 'ignore)))


;;
;; OS-integration
;;

(defun doom-open-with (&optional app-name path)
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

(defmacro def-open-with! (id &optional app dir)
  `(defun ,(intern (format "os-%s" id)) ()
     (interactive)
     (doom-open-with ,app ,dir)))

(def-open-with! open-in-default-program)
(def-open-with! open-in-browser "Google Chrome")
(def-open-with! reveal "Finder" default-directory)
(def-open-with! reveal-project "Finder" (doom/project-root))
(def-open-with! upload "Transmit")
(def-open-with! upload-folder "Transmit" default-directory)
(def-open-with! send-to-launchbar "LaunchBar")
(def-open-with! send-project-to-launchbar "LaunchBar" (doom/project-root))

(defun os-switch-to-term ()
  (interactive)
  (do-applescript "tell application \"iTerm\" to activate"))

(defun os-switch-to-term-and-cd ()
  (interactive)
  (doom:send-to-tmux (format "cd %s" (shell-quote-argument default-directory)))
  (doom-switch-to-iterm))

;; Open with external programs
(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations
        '(("\\.\\(pdf\\|jpe?g\\|gif\\|docx?\\|pptx?\\|xlsx?\\|zip\\|tar\\(\\.gz\\)?\\|rar\\)$"
           "open" (file)))))

(provide 'core-os-osx)
;;; core-os-osx.el ends here
