.POSIX:
.SUFFIXES:
.PHONY: all link unlink test clean help

TDIR  = ${HOME}/.emacs.d
FILES = emacs/.emacs.d

all: link

link:
	@/bin/sh etc/link_to_home_dir.sh

unlink:
	@unlink $(TDIR)

test:
	@diff -r $(TDIR) $(FILES)

clean:
	@rm -rf $(FILES)/.DS_Store      \
                .DS_Store               \
                emacs/.DS_Store         \
                etc/.DS_Store           \
                $(FILES)/auto-save-list \
                $(FILES)/bookmarks      \
                $(FILES)/custom.el      \
                $(FILES)/elpa           \
                $(FILES)/eshell         \
                $(FILES)/history        \
                $(FILES)/ido.last       \
                $(FILES)/init.elc       \
                $(FILES)/extras.elc     \
                $(FILES)/places         \
                $(FILES)/projects       \
                $(FILES)/transient      \
                $(FILES)/url

help:
	@/bin/sh etc/show_help.sh

