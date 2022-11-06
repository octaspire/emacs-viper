.PHONY: link unlink clean
TDIR=${HOME}/.emacs.d
FILES=emacs/.emacs.d

link:
	@sh etc/link_to_home_dir.sh

unlink:
	@unlink ${TDIR}

clean:
	@rm -rf ${FILES}/.DS_Store      \
                .DS_Store               \
                emacs/.DS_Store         \
                etc/.DS_Store           \
                ${FILES}/auto-save-list \
                ${FILES}/bookmarks      \
                ${FILES}/custom.el      \
                ${FILES}/elpa           \
                ${FILES}/eshell         \
                ${FILES}/history        \
                ${FILES}/ido.last       \
                ${FILES}/init.elc       \
                ${FILES}/extras.elc     \
                ${FILES}/places         \
                ${FILES}/projects       \
                ${FILES}/transient      \
                ${FILES}/url

