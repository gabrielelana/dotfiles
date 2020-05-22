# -*- mode: Shell-script[bash]; sh-basic-offset: 2; -*-

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=-1
HISTFILESIZE=-1

# Update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# ASDF configuration
[[ -d "$HOME/.asdf" ]] && {
  # shellcheck source=/home/coder/.asdf/asdf.sh
  source "$HOME/.asdf/asdf.sh"
  # shellcheck source=/home/coder/.asdf/completions/asdf.bash
  source "$HOME/.asdf/completions/asdf.bash"
}

# Cask configuration
[[ -d "$HOME/.cask/bin" ]] && {
  export PATH=$HOME/.cask/bin:$PATH
}

# EVM (Emacs Version Manager) configuration
[[ -d "$HOME/.evm/bin" ]] && {
  export PATH=$HOME/.evm/bin:$PATH
}

# Emacs compiled version
[[ -d "$HOME/opt/emacs" ]] && {
  export PATH=$HOME/opt/emacs/bin:$PATH
}

# Elixir language server configuration
[[ -d "$HOME/opt/elixir-ls" ]] && {
  # git clone https://github.com/elixir-lsp/elixir-ls ~/src/elixir-ls
  # cd ~/opt/elixir-ls
  # mix deps.get
  # MIX_ENV=prod mix elixir_ls.release -o ~/opt/elixir-ls
  export PATH=$HOME/opt/elixir-ls:$PATH
}

# Go configuration
[[ -d "$HOME/.go" ]] && {
  export GOPATH=$HOME/.go
  export PATH=$HOME/.go/bin:$PATH
}

# PHP configuration
[[ -d "$HOME/.composer" ]] && {
  export PATH=$HOME/.composer/vendor/bin:$PATH
}

# Heroku configuration
[[ -d "/usr/local/heroku/bin" ]] && {
  export PATH=/usr/local/heroku/bin:$PATH
}

# Direnv configuration
command -v direnv >/dev/null && {
  eval "$(direnv hook bash)"
}

# Rust configuration
[[ -d "$HOME/.cargo/bin" ]] && {
  export PATH=$HOME/.cargo/bin:$PATH
  # shellcheck source=/home/coder/.cargo/env
  source "$HOME/.cargo/env"
}

# Haskell (Stack) configuration
command -v stack >/dev/null && {
  eval "$(stack --bash-completion-script stack)"
  alias ghci="stack exec -- ghci"
  alias ghc="stack exec -- ghc"
}

# Prompt starship configuration
command -v starship >/dev/null && {
  eval "$(starship init bash)"
}

# Load all symbol font maps
for map in ~/.fonts/*.sh; do
  # shellcheck source=/dev/null
  source "$map"
done

export TERM="xterm-256color"
export EDITOR="emacs-client"
export PATH=$HOME/bin:$HOME/.local/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.local/lib:$LD_LIBRARY_PATH
export PYTHON="python2.7"

alias l='ls -CF'
alias la='ls -A'
alias ll='ls -la --color'
alias mongo="mongo --quiet"

# Load local configuration, aka configuration that is specific for the
# current machine
[[ -s "$HOME/.bashrc.localhost" ]] && {
  # shellcheck source=/home/coder/.bashrc.localhost
  source "$HOME/.bashrc.localhost"
}

# The following file is under version control so this is where you
# will put configuration that is stable
[[ -s "$HOME/.bashrc.$(hostname -d)" ]] && {
  # shellcheck source=/home/coder/.bashrc.localhost
  source "$HOME/.bashrc.$(hostname -d)"
}

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -n "$INSIDE_EMACS" ]; then
  export PS1='$ '
fi
