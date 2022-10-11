#
# Ensure we find ~/bin and the homebrew utilities
#
export PATH=/opt/homebrew/bin:$PATH
export PATH=/opt/homebrew/opt/mysql-client/bin:$PATH
export PATH=$HOME/Repos/github.com/skx/dotfiles/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/bin:$PATH


#
# Setup TAB-completion for ZSH
#
if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

  autoload -Uz compinit
  compinit
fi

#
# We need to configure the use of plugins for the pass password-manager.
#
# specifically I use the OTP plugin for two factor authentication for various services.
#
export PASSWORD_STORE_ENABLE_EXTENSIONS=true
export PASSWORD_STORE_EXTENSIONS_DIR=/usr/local/lib/password-store/extensions

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
