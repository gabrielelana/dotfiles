# -*- mode: Shell-script[bash]; sh-basic-offset: 2; -*-

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

# Make emacs available
[ -d "$HOME/opt/emacs-30.0.92/bin" ] && {
  export PATH=$HOME/opt/emacs-30.0.92/bin:$PATH
}
# [ -d "$HOME/opt/emacs-latest/bin" ] && {
#   export PATH=$HOME/opt/emacs-latest/bin:$PATH
# }

# Elixir language server configuration
[[ -d "$HOME/.local/bin/elixir-ls" ]] && {
  # git clone https://github.com/elixir-lsp/elixir-ls ~/opt/elixir-ls
  # cd ~/opt/elixir-ls
  # mix deps.get
  # MIX_ENV=prod mix elixir_ls.release -o ~/.local/bin/elixir-ls
  export ELS_INSTALL_PREFIX=$HOME/.local/bin/elixir-ls
  export PATH=$ELS_INSTALL_PREFIX:$PATH
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
command -v direnv >/dev/null && command -v asdf >/dev/null && {
  # Hook direnv into your shell.
  eval "$(asdf exec direnv hook bash)"

  # A shortcut for asdf managed direnv.
  direnv() { asdf exec direnv "$@"; }
}

# K8s configuration
command -v kubectl >/dev/null && {
  source <(kubectl completion bash)
  complete -F __start_kubectl k
  alias k=kubectl
}

# Rust configuration
[[ -d "$HOME/.cargo/bin" ]] && {
  export PATH=$HOME/.cargo/bin:$PATH
  source "$HOME/.cargo/env"
}

# Haskell configuration
[ -f "$HOME/.ghcup/env" ] && {
  source "/home/coder/.ghcup/env"
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
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
export AWS_DEFAULT_PROFILE=default
export GPG_TTY=$(tty)

alias l='ls -CF'
alias la='ls -A'
alias ll='ls -la --color'
alias mongo="mongo --quiet"
alias d="docker"
alias dc="docker compose"
alias python="python3"
alias kn="kubens"
alias k="kubectl"
alias tf="terraform"
alias tg="terragrunt"

# Load environment variables that should not be committed
[[ -f "$HOME/.env" ]] && {
  source "$HOME/.env"
}

# Load local configuration, aka configuration that is specific for the current
# machine
[[ -f "$HOME/.bashrc.localhost" ]] && {
  # shellcheck source=/home/coder/.bashrc.localhost
  source "$HOME/.bashrc.localhost"
}

# The following file is under version control so this is where you will put
# configuration that is stable
[[ -f "$HOME/.bashrc.$(hostname -d)" ]] && {
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
source "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/bashrc"
