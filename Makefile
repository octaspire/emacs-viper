.PHONY: clean stow unstow stow? unstow?
FILES=emacs/.emacs.d/
VERBOSE=1

stow:
	@stow --verbose ${VERBOSE} --target ${HOME} emacs

stow?:
	@stow --no --verbose ${VERBOSE} --target ${HOME} emacs

unstow:
	@stow --verbose ${VERBOSE} --target ${HOME} --delete emacs

unstow?:
	@stow --no --verbose ${VERBOSE} --target ${HOME} --delete emacs

clean:
	@rm -rf ${FILES}auto-save-list \
                ${FILES}custom.el      \
                ${FILES}elpa           \
                ${FILES}eshell         \
                ${FILES}history        \
                ${FILES}ido.last       \
                ${FILES}places         \
                ${FILES}transient      \
                ${FILES}url

