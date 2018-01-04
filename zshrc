# -*- mode: Shell-script; sh-basic-offset: 2; -*-

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="cc"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Load all symbol font maps
for map in ~/.fonts/*.sh; do source $map; done

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=('time' 'text' 'dotenv' 'chunkly' 'git' 'ruby' 'rails' 'rvm' 'gem' 'heroku' 'colors' 'nvm' 'composer' 'python-environment')

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
unsetopt correct_all

# Add local functions
fpath+=~/.zsh_functions

# Init autocompletion
compinit

# ASDF configuration
[[ -d "$HOME/.asdf" ]] && {
  source $HOME/.asdf/asdf.sh
  source $HOME/.asdf/completions/asdf.bash
}

# Rust configuration
[[ -d "$HOME/.cargo/bin" ]] && {
  export PATH=$PATH:$HOME/.cargo/bin
}

# Haskell configuration
[[ -d "/opt/ghc" ]] && {
  HASKELL_LATEST_VERSION=`ls /opt/ghc | sort -r | head -n1`
  HASKELL_ROOT="/opt/ghc/$HASKELL_LATEST_VERSION"
  CABAL_LATEST_VERSION=`ls /opt/cabal | sort -r | head -n1`
  CABAL_ROOT="/opt/cabal/$CABAL_LATEST_VERSION"
  export PATH=$HOME/.cabal/bin:$CABAL_ROOT/bin:$HASKELL_ROOT/bin:$PATH
}

# Heroku configuration
[[ -d "/usr/local/heroku/bin" ]] && {
  export PATH=$PATH:/usr/local/heroku/bin
}

# Direnv configuration
command -v direnv >/dev/null && {
  eval "$(direnv hook zsh)"
}

# Machine specific executables
[[ -d "$HOME/opt/bin" ]] && {
  export PATH=$PATH:$HOME/opt/bin
}

export TERM="xterm-256color"
export EDITOR="emacs-client"
export PATH=$HOME/bin:$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PYTHON="python2.7"

alias "."="cd ."
alias ".."="cd .."
{ local alias_from=".."
  local alias_to="cd .."
  for _pit in {1..42}; do
    alias_from="$alias_from."
    alias_to="$alias_to/.."
    eval "alias ${alias_from}=\"${alias_to}\""
  done
}

alias mongo="mongo --quiet"

alias npme='npm --registry http://registry.npmjs.eu'
alias vim-pure='vim -u ~/.dotfiles/vimrc-sensible --noplugin'
if `which ack-grep > /dev/null 2>&1`; then
  alias ack=ack-grep
fi

# Load local configuration, aka configuration that is specific for the
# current machine

# The following file is under version control so this is where you
# will put configuration that is stable
[[ -s "$HOME/.zshrc.`hostname -s`" ]] && {
  source $HOME/.zshrc.`hostname -s`
}

# The following file is *not* under version control so this is where
# you will put volatile configuration or configuration added by
# external tools
[[ -s "$HOME/.zshrc.localhost" ]] && {
  source $HOME/.zshrc.localhost
}
