all:
	@echo \'make install\' - install everything
	@echo \'make p\' - install prelude
	@echo \'make b\' - install bash environment
	@echo \'make e\' - install emacs environment

install: prelude bash emacs
	bash

p:
	export PRELUDE_URL="https://github.com/noahsug/prelude.git" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

b:
	mkdir -p ~/.bashrc.d
	touch bash/"$$(hostname).sh"
	-rm -r ~/.bashrc.d/sug
	ln -s $$(pwd)/bash ~/.bashrc.d/sug

e:
	-rm -r ~/.emacs.d/personal/sug
	-rm ~/.emacs.d/personal/sug.el
	ln -s $$(pwd)/emacs/sug.el ~/.emacs.d/personal/sug.el
	ln -s $$(pwd)/emacs/sug ~/.emacs.d/personal/sug
