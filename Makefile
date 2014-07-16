all:
	@echo \'make install\' - install everything
	@echo \'make b\' - install bash environment
	@echo \'make e\' - install emacs environment
	@echo \'make g\' - install git enpvironment
	@echo \'make s\' - install screen environment

install: b e g s
	bash

b:
	mkdir -p ~/.bashrc.d

	rm -rf ~/.bashrc.d/sugarman
	ln -s $$(pwd)/.bashrc.d/sugarman ~/.bashrc.d/sugarman

	rm -f ~/add_this_to_your_bashrc
	cp $$(pwd)/.bashrc ~/add_this_to_your_bashrc
	@echo Bash installed successfully! Please append the contents of the \'add_this_to_your_bashrc\' file to .bashrc.

e:
	mkdir -p ~/.emacs.d

	rm -rf ~/.emacs.d/sugarman
	ln -s $$(pwd)/.emacs.d/sugarman ~/.emacs.d/sugarman

	rm -rf ~/.emacs.d/elpa
	ln -s $$(pwd)/.emacs.d/elpa ~/.emacs.d/elpa

	rm -rf ~/.emacs.d/snippets
	ln -s $$(pwd)/.emacs.d/snippets ~/.emacs.d/snippets

	rm -rf ~/.emacs.d/insert
	ln -s $$(pwd)/.emacs.d/insert ~/.emacs.d/insert

	rm -f ~/add_this_to_your_dot_emacs
	cp $$(pwd)/.emacs ~/add_this_to_your_dot_emacs
	@echo Bash installed successfully! Please append the contents of the \'add_this_to_your_dot_emacs\' file to .emacs.

g:
	-rm ~/add_this_to_your_gitconfig
	cp $$(pwd)/.gitconfig ~/add_this_to_your_gitconfig
	@echo Bash installed successfully! Please append the contents of the \'add_this_to_your_gitconfig\' file to .gitconfig.

s:
	-rm ~/add_this_to_your_screenrc
	cp $$(pwd)/.screenrc ~/add_this_to_your_screenrc
	@echo Bash installed successfully! Please append the contents of the \'add_this_to_your_screenrc\' file to .screenrc.
