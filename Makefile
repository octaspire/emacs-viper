.PHONY: clean stow unstow stow? unstow?
FILES=emacs/.emacs.d/
VERBOSE=1

stow:
	@stow --verbose ${VERBOSE} --target ${HOME} emacs
	@/bin/bash etc/handle_bashrc.sh
stow?:
	@stow --no --verbose ${VERBOSE} --target ${HOME} emacs

unstow:
	@stow --verbose ${VERBOSE} --target ${HOME} --delete emacs

unstow?:
	@stow --no --verbose ${VERBOSE} --target ${HOME} --delete emacs

clean:
	@rm -rf ${FILES}auto-save-list \
		${FILES}transient      \
		${FILES}ido.last       \
		${FILES}places         \
		${FILES}history        \
		${FILES}elpa           \
		${FILES}eshell

