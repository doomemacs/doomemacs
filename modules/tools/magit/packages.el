;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "97a95f70079b6613bf98d2306279d3e03fe51234")
  (when (modulep! +forge)
    (package! forge :pin "ba35ffc9bafc6457cc95633904e53e34e544543f")
    (package! code-review
      :recipe (:files ("graphql" "code-review*.el"))
      :pin "26f426e99221a1f9356aabf874513e9105b68140")
    ; HACK closql c3b34a6ec438 breaks code-review wandersoncferreira/code-review#245,
    ; and the current forge commit (but forge does have an upstream fix),
    ; pinned as a temporary measure to prevent user breakages
    (package! closql :pin "0a7226331ff1f96142199915c0ac7940bac4afdd"))
  (package! magit-todos :pin "c6f3fd03aa5b750636c2647253f21cc03329566c"))
