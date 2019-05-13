;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(if (featurep! +wubi)
    (package! chinese-wbim)
  (package! pyim))
(package! fcitx)
(package! ace-pinyin)
(package! pangu-spacing)
