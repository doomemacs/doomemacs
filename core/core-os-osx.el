;;; core-os-osx.el --- Mac-specific settings

;; Use a shared clipboard
(setq x-select-enable-clipboard t
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ns-use-native-fullscreen nil
      ;; Don't open files from the workspace in a new frame
      ns-pop-up-frames nil
      ;; Prefixes: Command = M, Alt = A
      mac-command-modifier 'meta
      mac-option-modifier  'alt)

;; fix emacs PATH on OSX (GUI only)
(when window-system
  (setenv "SHELL" "/usr/local/bin/zsh")
  (setenv "EMACS" "1") ; make sure the world knows

  (setq exec-path (! (require 'exec-path-from-shell)
                     (exec-path-from-shell-initialize)
                     exec-path)))

;; OSX Related Plugins ;;;;;;;;;;;;;;;;;

(use-package applescript-mode :mode "\\.applescript$")

(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :config
  (add-to-list 'dash-at-point-mode-alist
               '(java-mode . "java,droid,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@after evil
  (when (featurep 'ns)
    ;; On OSX, stop copying each visual state move to the clipboard:
    ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
    ;; Most of this code grokked from:
    ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
    (defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
      (unless (featurep 'ns) ad-do-it))))

;; Send current file to OSX apps
(defun narf-open-with (&optional app-name path)
  (interactive)
  (let* ((path (f-full (s-replace "'" "\\'" (or path (if (eq major-mode 'dired-mode) (dired-get-file-for-visit) (buffer-file-name))))))
         (command (concat "open " (when app-name (concat "-a " (shell-quote-argument app-name))) " '" path "'")))
    (message "Running: %s" command)
    (shell-command command)))


(provide 'core-os-osx)
;;; core-os-osx.el ends here
