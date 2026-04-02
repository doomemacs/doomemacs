;;; ui/hl-todo/packages.el -*- lexical-binding: t; -*-

(use-package! hl-todo
  :hook (doom-first-buffer . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        ;; Don't highlight todo keywords in text-mode derivatives unless in
        ;; comments (e.g. data formats like yaml, json, etc).
        hl-todo-text-modes nil
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used intentionally
          ;; and is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project. Doom doesn't use BUG.
          ("BUG" error bold)
          ;; Doom uses XXX solely to highlight changes to the source in large
          ;; :override advice functions.
          ("XXX" font-lock-constant-face bold)
          ;; Doom uses NOTE to indicate either A) this comment is about a code
          ;; omission, e.g. "I *would've* put X here, but I didn't because Y",
          ;; or B) it's a comment about a large section of code beyond the scope
          ;; of adjacent lines.
          ("NOTE" success bold)))

  (defadvice! +hl-todo-clamp-font-lock-fontify-region-a (fn &rest args)
    "Fix an `args-out-of-range' error in some modes."
    :around #'hl-todo-mode
    (letf! (defun font-lock-fontify-region (beg end &optional loudly)
             (funcall font-lock-fontify-region (max beg 1) end loudly))
      (apply fn args)))

  ;; Use a more primitive todo-keyword detection method in major modes that
  ;; don't use/have a valid syntax table entry for comments.
  (add-hook! '(pug-mode-hook haml-mode-hook)
    (defun +hl-todo--use-face-detection-h ()
      "Use a different, more primitive method of locating todo keywords."
      (set (make-local-variable 'hl-todo-keywords)
           '(((lambda (limit)
                (let (case-fold-search)
                  (and (re-search-forward hl-todo-regexp limit t)
                       (memq 'font-lock-comment-face (ensure-list (get-text-property (point) 'face))))))
              (1 (hl-todo-get-face) t t))))
      (when hl-todo-mode
        (hl-todo-mode -1)
        (hl-todo-mode +1)))))
