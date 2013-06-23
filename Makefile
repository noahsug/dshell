all:
	@echo \'make install\' - install everything
	@echo \'make prelude\' - install prelude
	@echo \'make bash\' - install bash environment
	@echo \'make emacs\' - install emacs environment

install: prelude bash emacs
	bash

prelude:
	export PRELUDE_URL="https://github.com/noahsug/prelude.git" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

bash:
	mkdir -p ~/.bashrc.d
	touch bash/"$$(hostname).sh"
	-rm -r ~/.bashrc.d/sug
	ln -s $$(pwd)/bash ~/.bashrc.d/sug

emacs:
	-rm -r ~/.emacs.d/personal/sug
	-rm ~/.emacs.d/personal/sug.el
	ln -s $$(pwd)/emacs/sug.el ~/.emacs.d/personal/sug.el
	ln -s $$(pwd)/emacs/sug ~/.emacs.d/personal/sug
