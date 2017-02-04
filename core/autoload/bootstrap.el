;;; bootstrap.el --- for bootstrap scripts

;; This functionality is incomplete, do not use it.

(defvar doom-bootstrappers nil
  "")

;;;###autoload
(defun doom/bootstrap-module (module submodule)
  "TODO"
  (interactive
   (let* ((mod (completing-read "Select module: "
                                (mapcar (lambda (mod) (format "%s\t%s" (car mod) (cdr mod)))
                                        doom-modules)))
          (mods (s-split "\t" mod)))
     mods))
  (unless (and module submodule)
    (error "Aaaa %s %s" module submodule))
  (let ((path (doom-module-path module submodule)))
    ;;
    (message "Bootstrapping %s %s in %s" module submodule path)))

(defun doom--collect (rest)
  (let ((forms))
    (while (not (keywordp (car rest)))
      (push (pop rest) forms))
    (reverse forms)))

;;;###autoload
(defmacro bootstrap! (name desc &rest rest)
  ""
  (let ((argc (length rest))
        bs-exec
        bs-exec-after)
    (unless (= 0 (mod argc 2))
      (error "Incorrect number of arguments for bootstrap (%s)" argc))
    (while rest
      (let* ((arg (pop rest))
             (arg-name (symbol-name arg)))
        (cond ((eq arg :after)
               (add-to-list 'bs-exec-after (doom--collect rest) t))

              ((string-prefix-p ":if-" arg-name)
               (let ((os-name (substring arg-name (length ":if-")))
                     (os-sym (intern os-name))
                     (os-exec (doom--collect rest)))
                 (add-to-list 'bs-exec (cons os-sym os-exec) t))))))
    `(push (list ',name ,desc
                 (lambda ()
                   (letf (((symbol-function 'sh) (symbol-function 'bs-sh))
                          ((symbol-function 'sudo) (symbol-function 'bs-sudo))
                          ((symbol-function 'repo) (symbol-function 'bs-repo)))
                     ,@(mapcar (lambda (f)
                                 `(when (bs-is-os ',(car f))
                                    ,@(cdr os-exec)))
                               bs-exec)
                     ,@bs-exec-after)))
           doom-bootstrappers)))


;;;###autoload
(defun bs-repo (repo-url path)
  "Clone a remote version-controlled repo at REPO-URL to PATH, if it exists.
Requires the corresponding client, e.g. git for git repos, hg for mercurial,
etc.")

;;;###autoload
(defun bs-sh (command)
  "Runs a shell command and prints any output to the DOOM buffer.")

;;;###autoload
(defun bs-sudo (command)
  "Runs a shell command as admin, prompting for a password. Prints any output to
the DOOM buffer.")

;;;###autoload
(defun bs-is-os (os)
  "Rudimentary OS checking."
  (let ((gnu-linux-p (eq system-type 'gnu/linux))
        (os (if (symbolp os) (symbol-name os) os)))
    (pcase os
      ("macos"  (eq system-type 'darwin))
      ("linux"  gnu-linux-p)
      ("arch"   (and gnu-linux-pj(f-exists-p "/etc/arch-release")))
      ("debian" (and gnu-linux-p (f-exists-p "/etc/debian_version")))
      )))

(provide 'bootstrap)
;;; bootstrap.el ends here
