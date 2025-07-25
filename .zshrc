#
# Ensure we find ~/bin and the homebrew utilities
#
export PATH=/opt/homebrew/bin:$PATH
export PATH=$HOME/Repos/github.com/skx/dotfiles/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/bin:$PATH

export PYTHONDONTWRITEBYTECODE=1
#
#
#
export PASSWORD_STORE_ENABLE_EXTENSIONS=true
export PASSWORD_STORE_EXTENSIONS_DIR=/usr/local/lib/password-store/extensions


#
# Avoid homebrew cleanup, so we keep old things around.
#
export HOMEBREW_NO_INSTALL_CLEANUP=1

#
# If homebrew is installed ensure that it is used.
#
if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

#
# Load the completion for ZSH, along with the bash compatibility
#
autoload -U +X compinit     && compinit
autoload -U +X bashcompinit && bashcompinit


#
# I use atuin for shell-history
#
if type atuin &>/dev/null
then
  eval "$(atuin init zsh --disable-up-arrow)"
fi

#
# History control
#
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt SHARE_HISTORY
setopt appendhistory

#
# Alias to pipe stuff to less
#
#  ls LL
#
# And I'm spoiled by the use of 'rgrep' on Debian systems.
#
alias -g LL=' |& less'
alias -g rgrep='grep -R '

#
# If we have a local configuration-file, then load it.
#
if [ -e ~/.zshrc.local ] ; then
    source ~/.zshrc.local
fi

#
# Avoid Escape-backspace from deleting the previous token, instead
# delete components as with bash.
#
autoload -U select-word-style
select-word-style bash


#
# Ensure the comment-character works in interactive sessions.
#
setopt interactivecomments
export PATH="/opt/homebrew/sbin:$PATH"
