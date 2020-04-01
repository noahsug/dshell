alias g="git"
alias gsc="g show | grep '\(console.\|window.\)'"
alias gscl="g show | grep '\(console.\|window.\|git\)'"
alias gdc="g diff | grep '\(console.\|window.\)'"
alias gdcl="g diff | grep '\(console.\|window.\|git\)'"
alias gsd="g show | grep 'Debug\.log'"
alias gsdl="g show | grep '\(Debug\.log\|git\)'"

# current branch
gb() {
  git symbolic-ref --short HEAD
}

# merge-base
gmb() {
  git merge-base origin/master HEAD
}

# remote origin name
gremote() {
  git remote get-url origin | grep -oEi '\/(.+)\.git' | sed 's/\///' | sed 's/\.git//'
}

# compare branch
gcb() {
  echo 'https://git.musta.ch/airbnb/$(gremote)/compare/$(gb)' | xargs open -a 'Google Chrome'
}

gcm() {
  git checkout master
}

grs() {
  git reset HEAD~$1
}
gre() {
  git reset --hard HEAD~$1
}
grc() {
  git reset --hard HEAD~$1
  g clean -fd
}
grcm() {
  git reset --hard $(gmb)
  g clean -fd
}
grsm() {
  git reset $(gmb)
}

gdm() {
  git diff $(gmb)
}
gqdm() {
  git diff --name-only $(gmb)
}
grim() {
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

gcleanf() {
  gclean; g gc; g prune; g remote prune origin;
}

# check out latest branch (or 2nd latest if you're on latest)
# pass N to switch to the Nth branch
gcl() {
  n=$(([ -z "$1" ] && echo "0") || echo "$1")
  branch_1=$(git for-each-ref --format='%(refname:short)' refs/heads/ --sort=-committerdate | head -n "$(($n + 1))" | tail -n 1)
  branch_2=$(git for-each-ref --format='%(refname:short)' refs/heads/ --sort=-committerdate | head -n "$(($n + 2))" | tail -n 1)
  my_branch=$(gb)
  git checkout $(([ "$branch_1" == "$my_branch" ] && echo "$branch_2") || echo "$branch_1")
}

# git tmp branch - stash changes into tmp branch
gtb() {
  MY_GIT_BRANCH=$(gb)
  git branch -D tmp
  git checkout -b tmp
  git checkout $MY_GIT_BRANCH
}

# git tmp branch swap - swap changes with tmp
gtbs() {
  MY_GIT_BRANCH=$(gb)
  git branch -m "$MY_GIT_BRANCH"-tmp
  git branch -m tmp "$MY_GIT_BRANCH"
  git branch -m "$MY_GIT_BRANCH"-tmp tmp
  git checkout "$MY_GIT_BRANCH"
}

# fetch and rebase
# gfr() {
#   MY_GIT_BRANCH=$(gb)
#   git checkout master
#   git pull origin master
#   git checkout $MY_GIT_BRANCH
#   git rebase master
# }
# gfrp() {
#   MY_GIT_BRANCH=$(gb)
#   git checkout production
#   git fetch
#   git rebase
#   git checkout $MY_GIT_BRANCH
#   git rebase production
# }

# git start branch
# gsb() {
#   git checkout -b "$1" origin/master
# }
# gsbp() {
#   git checkout -b "$1" production
# }

# move commits from current branch to new branch : gsbm my-branch
#gsbm() {
#  sha=$(([ -z "$2" ] && echo "$(gmb)") || echo "HEAD~$2")
#  git branch $1
#  git reset --hard $sha
#  git checkout $1
#}

# g diff only changes
gdo() {
  b=$(([ -z $1 ] && echo "master") || echo $1)
  git diff $b | rg '^[+-][^+-]'
}

# gdo master additions
gdoam() {
  git diff master | rg '^[+][^+]'
}

# gdo master deletions
gdodm() {
  git diff master | rg '^[-][^-]'
}

# gdo master additions w/ filename
gdoafm() {
  git diff master | rg '^\+'
}

# gdo master deletions w/ filename
gdodfm() {
  git diff master | rg '^\-'
}

# gdo master changes w/ filename
gdofm() {
  git diff master | rg -e '^\-' -e '^\+'
}

# git reset grep - run 'g co master' on each matching file
#   e.g. greg 'app/.*jsx'
greg() {
  git st | rg "$1" -o | xargs -I % sh -c 'git co master %;'
}

## Rerun Git Tools
# export GIT_RERUN_DIR="$HOME/Dropbox/programming/code/git-rerun"
# [ -s "$GIT_RERUN_DIR/initialize.sh" ] && . "$GIT_RERUN_DIR/initialize.sh"

## Exec Rebase
# create or edit
ger() {
  p=$(git exec-rebase --path)
  echo "eo $p"
  if [ -f $p ]; then
    eo $p
  else
    eo $(git exec-rebase)
  fi
}
# update
geru() {
  p=$(git exec-rebase --path)
  cp $p "$p.backup" 2> /dev/null
  echo "eo $p"
  eo $(git exec-rebase)
}
# new (delete previous rebase file)
gern() {
  p=$(git exec-rebase --path)
  cp $p "$p.backup" 2> /dev/null
  rm -rf $p
  echo "eo $p"
  eo $(git exec-rebase)
}
# run (or create if rebase file doesn't exist)
gerr() {
  p=$(git exec-rebase --path)
  if [ -f $p ]; then
    git log --oneline -15
    git reset --hard $(git merge-base origin/master HEAD)
    git clean -fd
    . $p
  else
    echo "eo $p"
    eo $(git exec-rebase)
  fi
}
# continue (after resolving cherry-pick conflics)
gerc() {
  . $(git exec-rebase --continue)
}
# create tmp rebase file
gert() {
  p1=$(git exec-rebase --path)
  p2=$(git exec-rebase --name 'tmp.sh' --path)
  cp $p1 "$p2.backup" 2> /dev/null
  git exec-rebase
  cp $p1 $p2
  cp "$p2.backup" $p1 2> /dev/null
  echo "eo $p2"
  eo $p2
}

# git completion
source ~/.bashrc.d/sugarman/git-completion.bash
__git_complete g __git_main # allow alias "g" to complete
