# This is Dockerfile
# For build image run in directory with Dockerfile: docker build -t doomerror:0.1 .
# For run this image: docker run -it --rm doomerror:0.1
FROM alpine:3.17

# install emacs
RUN apk add emacs-x11-nativecomp

# for native compilation
RUN apk add bash gcc libc-dev

# Prerequisites for doomemacs
RUN apk add git ripgrep findutils fd 
RUN apk add grep graphviz markdown

WORKDIR /root

# Install doom-emacs
RUN git clone --depth 1 https://github.com/doomemacs/doomemacs emacs.d

ENV doom=~/.emacs.d/bin

RUN yes | emacs.d/bin/doom install

ENTRYPOINT emacs -nw