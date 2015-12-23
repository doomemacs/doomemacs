;;; init-org.el --- inits Emacs for Org-mode and writing
;;
;; This orgfile is sourced instead of init.el for instances of Emacs dedicated for
;; writing fiction, papers, or simply org-mode. I prefer to keep this functionality
;; separate from stock Emacs.
;;
;;;

(defconst EMACS-WRITE t)

(defconst narf-theme 'narf-light-theme)
(defconst narf-default-font (font-spec :family "Hack" :size 14))
(defconst narf-writing-font (font-spec :family "Deja Sans" :size 14))
(set-frame-font narf-default-font)

(let (file-name-handler-alist
      (gc-cons-threshold 169715200))
  (tool-bar-mode   -1)  ; no toolbar

  (load (concat user-emacs-directory "init-load-path.el"))
  (load-theme narf-theme t)

  (mapc 'require
        `(core ; core/core.el

          core-popup           ; taming stray windows
          core-ui              ; draw me like one of your French editors
          core-evil            ; come to the dark side, we have cookies
          core-editor          ; filling the editor-shaped hole in the emacs OS
          core-company         ; for the lazy typist
          core-yasnippet       ; for the lazier typist
          core-auto-insert     ; for the laziest typist
          core-flycheck        ; remember that semicolon you forgot?
          core-project         ; whose project am I in?
          core-vcs             ; version control is a programmer's best friend
          core-helm            ; a search engine for life and love
          core-eval            ; run code, run.
          core-workgroups      ; cure Emacs alzheimers

          ;;; Environments
          module-markdown
          module-org
          module-plantuml

          ;;; Specific custom functionality
          ;; lib-demo
          ;; lib-writing
          ;; lib-tmux
          ;; lib-crm
          ;; lib-sonicpi

          ;;; Key bindings & ex commands
          my-bindings
          my-commands
          ))
  (narf-init))

(setq wg-session-file (expand-file-name "wg-org-default" narf-temp-dir))

