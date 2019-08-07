;;; ui/symbol-overlay/config.el -*- lexical-binding: t; -*-

(def-package! symbol-overlay
  :config
  (map! :leader
        "c o" 'symbol-overlay-put)
  (map! :map symbol-overlay-map
        [escape] 'symbol-overlay-put))
