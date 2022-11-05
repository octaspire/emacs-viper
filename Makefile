.PHONY: link unlink clean
TDIR=${HOME}/.emacs.d
FILES=emacs/.emacs.d

link:
	@ln -s ${PWD}/${FILES}/ ${TDIR}

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

