;;; lang/sh/boostrap.el

(bootstrap!
 :title "{z,ba}sh"
 :desc "Sets up the zshdb and bashdb debuggers, and shell-check"

 :if-debian
 (sudo "apt-get update && apt-get install zshdb bashdb spellcheck")

 :if-arch
 (sudo "pacman --noconfirm --needed -S zshdb bashdb shellcheck")

 :if-macos
 (sh "brew install zshdb bashdb"))
