# Go

Go support, including auto-completion (gocode), eldoc support (go-eldoc), REPL
support (gore), refactoring commands (gorename), syntax-checking (flycheck),
auto-formatting (gofmt), and snippets (yasnippet).

Snippets can be found in `private/hlissner/snippets/go-mode`.

## External Dependencies

Run `make bootstrap go` to install these.

+ Go (`brew install go`, `pacman -S go`)
+ gocode `go get -u github.com/nsf/gocode` (completion)
+ gore `go get -u github.com/motemen/gore` (REPL)
+ guru `golang.org/x/tools/cmd/guru` (code navigation commands)
+ gorename `golang.org/x/tools/cmd/gorename` (refactoring commands)

