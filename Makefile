
all: install update

install:
	@cask install

update:
	@cask update

clean:
	@rm -f init.elc {core,modules,private,contrib}/*.elc
