;;; lang/org/+protocol.el -*- lexical-binding: t; -*-

;; Brings lazy-loaded support for org-protocol, so external programs (like
;; browsers) can invoke specialized behavior from Emacs. Normally you'd simply
;; require `org-protocol' and use it, but the package loads all of org for no
;; compelling reason, so...
(defun +org*server-visit-files (args)
  "Advise `server-visit-flist' to invoke `org-protocol' lazily."
  (cl-destructuring-bind (files proc &optional nowait) args
    (catch 'greedy
      (let ((flist (reverse files)))
        (dolist (var flist)
          (when (string-match-p ":/+" (car var))
            (require 'server)
            (require 'org-protocol)
            ;; `\' to `/' on windows
            (let ((fname (org-protocol-check-filename-for-protocol
                          (expand-file-name (car var))
                          (member var flist)
                          proc)))
              (cond ((eq fname t) ; greedy? We need the t return value.
                     (setq files nil)
                     (throw 'greedy t))
                    ((stringp fname) ; probably filename
                     (setcar var fname))
                    ((setq files (delq var files)))))))))
    (list files proc nowait)))
(advice-add #'server-visit-files :filter-args #'+org*server-visit-files)

;; Disable built-in, clumsy advice
(after! org-protocol
  (ad-disable-advice 'server-visit-files 'before 'org-protocol-detect-protocol-server))


;; TODO org-board or better link grabbing support
;; TODO org-capture + org-protocol instead of bin/org-capture
