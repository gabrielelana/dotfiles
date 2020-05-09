#!/bin/bash

# TODO: install latest version of language && language server && utilities for: Elixir
# TODO: install latest version of language && language server && utilities for: Rust
# TODO: install latest version of language && language server && utilities for: JS/TS
# TODO: add to switch-theme a light theme that works
# TODO: Firefox minimal theme?
# TODO: Firefox theme customization with the same colors in switch-theme?
# TODO: how to share custom dictionary in Emacs
# TODO: remove zsh things and references
# TODO: remove chunkly things and references
# TODO: remove slim things and references
# TODO: how to have non intrusive notifications in i3?

echo "Setup system..."
# To run the current script you need to have
# - Installed an Ubuntu desktop with an user X
# - You are logged in with that user X which needs to be a sudoers
# - You have both private and public SSH keys in ~/.ssh and the
#   following configuration to be able to checkout private
#   repositories from Github for later
#
#   $ cat <<EOF > ~/.ssh/config
#     Host *
#     ServerAliveInterval 60
#     ConnectTimeout 1
#
#     Host github.com
#       User git
#       IdentityFile ~/.ssh/id_rsa.gabrielelana
#       StrictHostKeyChecking no
#       HostName github.com
#     EOF
#
# After this script ran the you need to logout and at the next login
# you must choose instead of Ubuntu the XSession to read the
# ~/.xsession file and therefore to be able to run i3wm
#
# Then you run ~/.dotfiles/bootstrap-profile.sh

echo "Update package repositories..."
sudo apt update

echo "Install basic packages..."
sudo apt install -y \
     aspell-it \
     autoconf \
     autocutsel \
     binutils \
     bison \
     build-essential \
     compton \
     coreutils \
     curl \
     dbus-x11 \
     docker \
     dunst \
     entr \
     flex \
     fop \
     g++-8 \
     gcc-multilib \
     git \
     gnutls-bin \
     hsetroot \
     i3 \
     iitalian \
     jq \
     libcanberra-gtk3-module \
     libgif-dev \
     libgl1-mesa-dev \
     libgl1-mesa-glx \
     libglu1-mesa-dev \
     libgmp-dev \
     libgnutls28-dev \
     libgtk-3-dev \
     libicu-dev \
     libjansson-dev \
     libjansson4 \
     libjpeg-dev \
     libmpc-dev \
     libmpfr-dev \
     libncurses-dev \
     libncurses5-dev \
     libpng-dev \
     libreadline-dev \
     libssl-dev \
     libtiff-dev \
     libtiff5-dev \
     libtinfo5 \
     libwebkit2gtk-4.0-dev \
     libwxgtk3.0-dev \
     libxaw7-dev \
     libxft-dev \
     libxml2-dev \
     libxpm-dev \
     libxt-dev \
     make \
     silversearcher-ag \
     slim \
     suckless-tools \
     texinfo \
     unclutter \
     unixodbc-dev \
     unzip \
     vim \
     xsltproc \
     zsh

echo "Cloning Emacs repository, be patient..."
mkdir -p ~/tmp && cd ~/tmp || exit 1
git clone git://git.sv.gnu.org/emacs.git emacs-build && cd emacs-build || exit 1
EMACS_RELEASE=$(git describe --abbrev=0 --tags)
echo "Compile and install Emacs $EMACS_RELEASE be patient..."
git checkout "$EMACS_RELEASE"
./autogen.sh && \
  ./configure --prefix=/home/coder/opt/emacs --bindir=/home/coder/opt/emacs/bin \
              --with-modules \
              --with-x=yes \
              --with-x-toolkit=gtk3 \
              --with-xwidgets \
              --without-dbus && \
  make -j"$(nproc)" && \
  make install
# Cask
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
# EVM
sudo mkdir /usr/local/evm
sudo chown "$USER": /usr/local/evm
curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash

echo "Install ShellCheck..."
mkdir -p ~/bin
if [ ! -f ~/bin/pup ]; then
  mkdir -p ~/tmp && cd ~/tmp || exit 1
  wget --quiet "https://storage.googleapis.com/shellcheck/shellcheck-stable.linux.x86_64.tar.xz"
  tar --xz -xvf shellcheck-stable.linux.x86_64.tar.xz
  cp shellcheck-stable/shellcheck ~/bin/shellcheck
  rm -rf shellcheck-stable
  rm -f shellcheck-stable.linux.x86_64.tar.xz
fi

echo "Install pup..."
mkdir -p ~/bin
if [ ! -f ~/bin/pup ]; then
  cd ~/tmp || exit 1
  PUP_VERSION=0.4.0
  wget -q https://github.com/ericchiang/pup/releases/download/v${PUP_VERSION}/pup_v${PUP_VERSION}_linux_amd64.zip
  unzip pup_v${PUP_VERSION}_linux_amd64.zip
  mv pup ~/bin
  rm -rf pup_v${PUP_VERSION}_linux_amd64.zip
fi

echo "Install MongoDB shell extensions..."
if [ ! -s ~/.mongorc.js ]; then
  curl -sL https://raw.github.com/gabrielelana/mongodb-shell-extensions/master/released/mongorc.js > ~/.mongorc.js
fi

echo "Install ASDF and related plugins..."
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
cd ~/.asdf && git checkout "$(git describe --abbrev=0 --tags)" && cd - || exit 1
~/.asdf/bin/asdf plugin-add mongodb
~/.asdf/bin/asdf plugin-add postgres
~/.asdf/bin/asdf plugin-add erlang
~/.asdf/bin/asdf plugin-add elixir
~/.asdf/bin/asdf plugin-add ocaml
~/.asdf/bin/asdf plugin-add opam
~/.asdf/bin/asdf plugin-add ruby
~/.asdf/bin/asdf plugin-add php
~/.asdf/bin/asdf plugin-add nodejs
~/.asdf/bin/asdf plugin-add golang
~/.asdf/bin/asdf plugin-add java
~/.asdf/bin/asdf plugin-add sbt
~/.asdf/bin/asdf plugin-add scala

echo "Install Rust..."
curl https://sh.rustup.rs -sSf | sh -s -- --no-modify-path -y

echo "Install Haskell Stack..."
curl -sSL https://get.haskellstack.org/ | sh -s - -f
# To install ghcide follow the following instructions (https://github.com/digital-asset/ghcide)
# (better to do manually because it doesn't look stable enough to automate)
# $ git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
# $ cd haskell-ide-engine
# $ stack ./install.hs hie

echo "Install Starship..."
curl -fsSL https://starship.rs/install.sh | bash
