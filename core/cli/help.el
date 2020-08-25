;;; core/cli/help.el -*- lexical-binding: t; -*-

(defun doom--cli-print-signature (cli)
  (print! (bold "Usage: doom %s%s%s")
          (if (doom-cli-internal-p cli)
              ""
            (concat (doom-cli-name cli) " "))
          (if-let* ((optlist (doom-cli-optlist cli))
                    (flags (cl-loop for opt in optlist
                                    append (doom-cli-option-flags opt)))
                    (fn (doom-partial #'string-prefix-p "--")))
              (concat (when-let (short-flags (cl-remove-if fn flags))
                        ;; TODO Show arguments of short flags
                        (format "[-%s]"
                                (string-join (mapcar (doom-rpartial #'substring 1 nil) short-flags)
                                             "")))
                      ;; TODO Show long flags
                      ;; (when-let (long-flags (cl-remove-if-not fn flags))
                      ;;   (concat " " (string-join long-flags " ")))
                      " ")
            "")
          (if-let (arglist (doom-cli-arglist cli))
              (string-join (append (cl-loop for arg in arglist
                                            until (memq arg cl--lambda-list-keywords)
                                            collect (upcase (symbol-name arg)))
                                   (cl-loop for arg in (cdr (memq '&optional arglist))
                                            until (memq arg cl--lambda-list-keywords)
                                            collect (format "[%s]" (upcase (symbol-name arg)))))
                           " ")
            ""))
  (when-let (aliases (doom-cli-aliases cli))
    (print! "Aliases: %s" (string-join aliases ", "))))

(defun doom--cli-print-desc (cli &optional short)
  (print! "%s"
          (if short
              (car (split-string (doom-cli-desc cli) "\n"))
            (doom-cli-desc cli))))

(defun doom--cli-print-short-desc (cli)
  (doom--cli-print-desc cli 'short))

(defun doom--cli-print-options (cli)
  (when-let (optlist (doom-cli-optlist cli))
    (print! (bold "Options:"))
    (print-group!
     (cl-loop for opt in optlist
              for desc = (doom-cli-option-desc opt)
              for args = (doom-cli-option-args opt)
              for flagstr = (string-join (doom-cli-option-flags opt) ", ")
              do
              ;; TODO Adjust columns dynamically
              (print! "%-18s"
                      (concat flagstr
                              (when-let (arg (car args))
                                (concat " " (upcase (symbol-name arg))))))
              (print-group!
               (print! (autofill "%s") desc))))))


(defun doom--cli-print (cli)
  (doom--cli-print-signature cli)
  (terpri)
  (doom--cli-print-desc cli)
  (terpri)
  (doom--cli-print-options cli))


;;
;;; Commands

(defcli! (help h) (&optional command)
  "Describe a command or list them all."
  :bare t
  (if command
      (doom--cli-print (doom-cli-get (intern command)))
    (doom--cli-print (doom-cli-get :doom))
    (terpri)
    (print! (bold "Commands:"))
    (print-group!
     (dolist (group (seq-group-by (lambda (cli)
                                    (plist-get (doom-cli-plist cli) :group))
                                  (cl-loop for name being the hash-keys of doom--cli-commands
                                           for cli = (gethash name doom--cli-commands)
                                           if (and (doom-cli-p cli)
                                                   (not (doom-cli-internal-p cli))
                                                   (not (plist-get (doom-cli-plist cli) :hidden)))
                                           collect cli)))
       (if (null (car group))
           (dolist (cli (cdr group))
             (print! "%-16s %s"
                     (doom-cli-name cli)
                     (car (split-string (doom-cli-desc cli) "\n"))))
         (print! "%-26s %s"
                 (bold (concat (car group) ":"))
                 (gethash (car group) doom--cli-groups))
         (print-group!
          (dolist (cli (cdr group))
            (print! "%-16s %s"
                    (doom-cli-name cli)
                    (car (split-string (doom-cli-desc cli) "\n"))))))
       (terpri)))))
