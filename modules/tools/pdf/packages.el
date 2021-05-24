;; -*- no-byte-compile: t; -*-
;;; tools/pdf/packages.el

(package! pdf-tools
  :recipe (:host github
           :repo "vedang/pdf-tools"
           :post-build
           (let ((warning-minimum-log-level :error))
             (require 'pdf-tools)
             (unless (file-exists-p pdf-info-epdfinfo-program)
               (require 'pdf-occur)
               (print-group!
                (print! (start "Building epdfinfo for pdf-tools"))
                (with-current-buffer (pdf-tools-install 'no-query)
                  (while compilation-in-progress
                    (sleep-for 1))
                  (when (> compilation-num-errors-found 0)
                    (print! (warn "Failed to build epdfinfo because: %s" (buffer-string)))))))))
  :pin "d262cf9e19d57c6567e06e51d109150c20753839")

(package! saveplace-pdf-view :pin "54ed966b842501c3c092dbf57b372e37b033c578")
