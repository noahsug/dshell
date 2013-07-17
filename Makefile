all:
	@echo \'make i\' - install everything
	@echo \'make s\' - install everything except prelude
	@echo \'make p\' - install prelude
	@echo \'make b\' - install bash environment
	@echo \'make e\' - install emacs environment
	@echo \'make g\' - install git environment

s: b e g
	bash 

i: p b e g
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
	mkdir -p ~/.emacs.d/personal
	ln -s $$(pwd)/emacs/sug.el ~/.emacs.d/personal/sug.el
	ln -s $$(pwd)/emacs/sug ~/.emacs.d/personal/sug

g:
	-mv ~/.gitconfig ~/.gitconfig_backup
	ln -s $$(pwd)/.gitconfig ~/