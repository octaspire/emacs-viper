.PHONY: link unlink clean
TDIR=${HOME}/.emacs.d
FILES=emacs/.emacs.d

link:
	@sh etc/link_to_home_dir.sh

unlink:
	@unlink ${TDIR}

clean:
	@rm -rf ${FILES}/auto-save-list \
                ${FILES}/custom.el      \
                ${FILES}/elpa           \
                ${FILES}/eshell         \
                ${FILES}/history        \
                ${FILES}/ido.last       \
                ${FILES}/places         \
                ${FILES}/projects       \
                ${FILES}/transient      \
                ${FILES}/url

