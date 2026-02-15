;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/vhdl/doctor.el

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

;; LSP
(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(when (modulep! +lsp)
  (let ((vhdl-servers '("vhdl_ls" "vhdl-tool" "hdl_checker" "ghdl-ls")))
    (unless (seq-some #'executable-find vhdl-servers)
      (warn! "Couldn't find any VHDL language server in your PATH. Tried: %s"
             (string-join vhdl-servers ", ")))))
