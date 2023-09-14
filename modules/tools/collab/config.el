;;; tools/collab/config.el -*- lexical-binding: t; -*-

(use-package! crdt
  :commands (crdt-share-buffer crdt-connect)
  :init
  (when (modulep! +tunnel)
    (setq crdt-use-tuntox t)
    (setq crdt-tuntox-password-in-url t)))
