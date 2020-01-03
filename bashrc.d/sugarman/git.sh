alias g="git"
alias gsc="g show | grep '\(console.\|window.\)'"
alias gscl="g show | grep '\(console.\|window.\|git\)'"
alias gdc="g diff | grep '\(console.\|window.\)'"
alias gdcl="g diff | grep '\(console.\|window.\|git\)'"
alias gsd="g show | grep 'Debug\.log'"
alias gsdl="g show | grep '\(Debug\.log\|git\)'"

# current branch
function gb() {
  git symbolic-ref --short HEAD
}

# merge-base
function gmb() {
  git merge-base origin/master HEAD
}

# remote origin name
function gremote() {
  git remote get-url origin | grep -oEi '\/(.+)\.git' | sed 's/\///' | sed 's/\.git//'
}

# compare branch
function gcb() {
  echo 'https://git.musta.ch/airbnb/$(gremote)/compare/$(gb)' | xargs open -a 'Google Chrome'
}

function gcm() {
  git checkout master
}

function grs() {
  git reset HEAD~$1
}
function gre() {
  git reset --hard HEAD~$1
}
function grc() {
  git reset --hard HEAD~$1
  g clean -fd
}
function grcm() {
  git reset --hard $(gmb)
  g clean -fd
}
function grsm() {
  git reset $(gmb)
}

function gdm() {
  git diff $(gmb)
}
function gqdm() {
  git diff --name-only $(gmb)
}
function grim() {
  git rebase -i $(gmb)
}

# git diff-tree additions
alias gdta="diff-tree | rg '^A| [^.]+$'"
# alias gf="g add . ; g cm -am 'fix up'"
# alias gfa="g add . ; g cm -a --amend -C HEAD"
# alias gfm="g cm -m 'fix up'"
# alias gfma="g cm --amend -C HEAD"
# alias gfp='gf ; g p'
# alias ge='g pf'
# alias gfe='gf ; g pf'
# alias gfap='gfa ; g p'
# alias gfae='gfa ; g pf'

# fuzzy git
alias gfc='git-fuzzy-checkout'

# remove dead branches
alias gclean='git-clean.js'

function gcleanf() {
  gclean; g gc; g prune; g remote prune origin;
}

# check out latest branch (or 2nd latest if you're on latest)
# pass N to switch to the Nth branch
function gcl() {
  n=$(([ -z "$1" ] && echo "1") || echo "$1")
  branch_1=$(git for-each-ref --format='%(refname:short)' refs/heads/ --sort=-committerdate | head -n "$n" | tail -n 1)
  branch_2=$(git for-each-ref --format='%(refname:short)' refs/heads/ --sort=-committerdate | head -n "$(($n + 1))" | tail -n 1)
  my_branch=$(gb)
  git checkout $(([ "$branch_1" == "$my_branch" ] && echo "$branch_2") || echo "$branch_1")
}

# git tmp branch - stash changes into tmp branch
function gtb() {
  MY_GIT_BRANCH=$(gb)
  git branch -D tmp
  git checkout -b tmp
  git checkout $MY_GIT_BRANCH
}

# git tmp branch swap - swap changes with tmp
function gtbs() {
  MY_GIT_BRANCH=$(gb)
  git branch -m "$MY_GIT_BRANCH"-tmp
  git branch -m tmp "$MY_GIT_BRANCH"
  git branch -m "$MY_GIT_BRANCH"-tmp tmp
  git checkout "$MY_GIT_BRANCH"
}

# fetch and rebase
# function gfr() {
#   MY_GIT_BRANCH=$(gb)
#   git checkout master
#   git pull origin master
#   git checkout $MY_GIT_BRANCH
#   git rebase master
# }
# function gfrp() {
#   MY_GIT_BRANCH=$(gb)
#   git checkout production
#   git fetch
#   git rebase
#   git checkout $MY_GIT_BRANCH
#   git rebase production
# }

# git start branch
# function gsb() {
#   git checkout -b "$1" origin/master
# }
# function gsbp() {
#   git checkout -b "$1" production
# }

# move commits from current branch to new branch : gsbm my-branch
#function gsbm() {
#  sha=$(([ -z "$2" ] && echo "$(gmb)") || echo "HEAD~$2")
#  git branch $1
#  git reset --hard $sha
#  git checkout $1
#}

# g diff only changes
function gdo() {
  b=$(([ -z $1 ] && echo "master") || echo $1)
  git diff $b | rg '^[+-][^+-]'
}

# gdo master additions
function gdoam() {
  git diff master | rg '^[+][^+]'
}

# gdo master deletions
function gdodm() {
  git diff master | rg '^[-][^-]'
}

# gdo master additions w/ filename
function gdoafm() {
  git diff master | rg '^\+'
}

# gdo master deletions w/ filename
function gdodfm() {
  git diff master | rg '^\-'
}

# gdo master changes w/ filename
function gdofm() {
  git diff master | rg -e '^\-' -e '^\+'
}

# git reset grep - run 'g co master' on each matching file
#   e.g. greg 'app/.*jsx'
function greg() {
  git st | rg "$1" -o | xargs -I % sh -c 'git co master %;'
}

## Helper functions

# git completion
source ~/.bashrc.d/sugarman/git-completion.bash
__git_complete g __git_main # allow alias "g" to complete
