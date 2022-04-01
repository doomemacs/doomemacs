;;; core/cli/debug.el -*- lexical-binding: t; -*-

(load! "autoload/debug" doom-core-dir)

;;
;;; Commands

(defcli! info
    ((format ["--json" "--md" "--lisp"] "What format to dump info into"))
  "Output system info in markdown for bug reports."
  (with-temp-buffer
    (pcase format
      ("--json"
       (require 'json)
       (insert (json-encode (doom-info)))
       (json-pretty-print-buffer))
      ("--lisp"
       (pp (doom-info)))
      (`nil
       (dolist (spec (cl-remove-if-not #'cdr (doom-info)))
         (insert! "%-11s  %s\n"
                  ((car spec)
                   (if (listp (cdr spec))
                       (mapconcat (lambda (x) (format "%s" x))
                                  (cdr spec) " ")
                     (cdr spec))))))
      (_
       (user-error "I don't understand %S. Did you mean --json, --md/--markdown or --lisp?"
                   format)))
    (print! (buffer-string)))
  nil)

(defcli! (version v) ()
  "Show version information for Doom & Emacs."
  (doom/version)
  nil)

(defcli! amisecure ()
  "TODO"
  (unless (string-match-p "\\_<GNUTLS\\_>" system-configuration-features)
    (warn "gnutls support isn't built into Emacs, there may be problems"))
  (if-let* ((bad-hosts
             (cl-loop for bad
                      in '("https://expired.badssl.com/"
                           "https://wrong.host.badssl.com/"
                           "https://self-signed.badssl.com/"
                           "https://untrusted-root.badssl.com/"
                           ;; "https://revoked.badssl.com/"
                           ;; "https://pinning-test.badssl.com/"
                           "https://sha1-intermediate.badssl.com/"
                           "https://rc4-md5.badssl.com/"
                           "https://rc4.badssl.com/"
                           "https://3des.badssl.com/"
                           "https://null.badssl.com/"
                           "https://sha1-intermediate.badssl.com/"
                           ;; "https://client-cert-missing.badssl.com/"
                           "https://dh480.badssl.com/"
                           "https://dh512.badssl.com/"
                           "https://dh-small-subgroup.badssl.com/"
                           "https://dh-composite.badssl.com/"
                           "https://invalid-expected-sct.badssl.com/"
                           ;; "https://no-sct.badssl.com/"
                           ;; "https://mixed-script.badssl.com/"
                           ;; "https://very.badssl.com/"
                           "https://subdomain.preloaded-hsts.badssl.com/"
                           "https://superfish.badssl.com/"
                           "https://edellroot.badssl.com/"
                           "https://dsdtestprovider.badssl.com/"
                           "https://preact-cli.badssl.com/"
                           "https://webpack-dev-server.badssl.com/"
                           "https://captive-portal.badssl.com/"
                           "https://mitm-software.badssl.com/"
                           "https://sha1-2016.badssl.com/"
                           "https://sha1-2017.badssl.com/")
                      if (condition-case _e
                             (url-retrieve-synchronously bad)
                           (error nil))
                      collect bad)))
      (print! (error "tls seems to be misconfigured (it got %s).")
              bad-hosts)
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (print! (warn "Something went wrong.\n\n%s") (pp-to-string status))
                      (print! (success "Your trust roots are set up properly.\n\n%s") (pp-to-string status))
                      t)))))
