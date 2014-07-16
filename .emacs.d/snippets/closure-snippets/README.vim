== Prerequisites ==

Before proceeding, make sure you have the latest version of
XPTemplate [1] installed and configured.

1. http://code.google.com/p/xptemplate/

== Installation ==

Drop closure.xpt.vim into one of the following:

~/vimfiles/personal/ftplugin/javascript/    (recommended)
or
~/vimfiles/ftplugin/javascript/

Snippet priority is set to lang-2 by default, giving it
priority over XPTemplate js templates in the case of
conflict (on subclass for example). You can adjust this
priority by altering the first line of the file as
appropriate.
