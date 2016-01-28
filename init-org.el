;;; init-org.el --- inits Emacs for Org-mode and writing
;;
;; This orgfile is sourced instead of init.el for instances of Emacs dedicated for
;; writing fiction, papers, or simply org-mode. I prefer to keep this functionality
;; separate from stock Emacs.
;;
;;;

(defconst EMACS-WRITE t)

(defconst narf-theme 'narf-light)
(defconst narf-default-font (font-spec :family "Hack" :size 14))
(defconst narf-writing-font (font-spec :family "Deja Sans" :size 14))

(defconst narf-packages
  '(core ; core/core.el

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
    ;; lib-plugin        ; plugin dev for various programs
    lib-tmux             ; closing the rift between GUI & terminal
    lib-demo             ; let me demonstrate...
    lib-writing          ; yes, I write papers and fiction in emacs
    ;; lib-crm           ; emacs and org-mode based CRM

          ;;; Key bindings & ex commands
    my-bindings
    my-commands
    ))

(load (concat user-emacs-directory "init-packages.el"))

(setq wg-session-file (expand-file-name "wg-org-default" narf-temp-dir))
(cd org-directory)

