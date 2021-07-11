;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "359347b2bb15f8d7ef819692ac79759ccfe2c85d")
(package! markdown-toc :pin "f78cba9b5761c91058fed3a781bd3fed7f996e1f")
(package! edit-indirect :pin "bdc8f542fe8430ba55f9a24a7910639d4c434422")

(when (featurep! +grip)
  (package! grip-mode :pin "c0ca78990395245e5f742166047b04eeff63cf6a"))

(when (featurep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "064fe9b4767470472356d20bdd08e2f30ebbc9ac"))
