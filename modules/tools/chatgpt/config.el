;;; config.el -*- lexical-binding: t; -*-

(after! chat
  (setq chat-api-key (auth-source-pick-first-password :host "api.openai.com"))
  (add-hook! '(chat-query-mode-hook chat-mode-hook)
	     '((lambda ()
		 (toggle-truncate-lines 0))
	       (lambda ()
		 (flyspell-mode 1)))))

(setq chat-bot-prompt "GPT > ")

(map!
 :map evilem-map
 :desc "chatgpt"
 "c" #'chat-query-dwim)

(map!
 :mode chat-query-mode
 :localleader
 :desc "Reply" "r" #'chat-query-reply)

(map!
 :leader
 :prefix "o"
 :desc "chatgpt"
 "c" #'chat)
