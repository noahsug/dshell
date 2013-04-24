# scala
export PATH="/usr/local/bin:/usr/local/scala/scala-2.9.2/bin:$PATH"

# npm installs
export PATH="/usr/local/share/npm/bin:$PATH"

# bash completion
if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
    || complete -o default -o nospace -F _git g

# ls colors
export LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'
export CLICOLOR=1
export LSCOLORS=dxhxcxfxbxegedabagaced

# PS1
case $TERM in
    dumb)
        export PS1='\W\$ '
        ;;
    *)
        export PS1='\[\e[38;5;187m\]\w\[\e[38;5;174m\]$(__git_ps1 "[%s]") \[\e[m\]'
        ;;
esac

# cd + ls -- WARNING change this to cl when scripts depend on cd having no output
function cd() {
    builtin cd "$1"
    ls -A
}

# diff deleted (assuming first file is original)
function did() {
    diff "$1" "$2" | grep ">"
}

# diff added (assuming first file is origonal)
function dia() {
    diff "$1" "$2" | grep "<"
}

# emacs stuff
emacs --daemon
EDITOR="emacsclient -t"
VISUAL="emacsclient -t"

# alias
alias g5="git5"
alias g="git"
alias e="emacsclient -t -a emacs -nw"
alias en="killall emacs ; emacs --daemon ; emacsclient -t -a emacs -nw"
alias kille="killall emacs"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ~="cd ~"
alias la="ls -A"
alias c="clear"
alias di="diff"

# search through history on C-P, C-N
bind "C-p":history-search-backward
bind "C-n":history-search-forward

# edit shortcuts
alias 0ebrc="e ~/.bashrc"
alias 0eb="e ~/.bashrc.d/sug/main.sh"
alias 0ee="e ~/.emacs.d/personal/sug/main.el"
alias 0eg="e ~/.gitconfig"

. ~/.bashrc.d/sug/"$(hostname).sh"
