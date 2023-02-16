#!/bin/bash

# TODO: break the following sections in single executables
# - bootstrap-ubuntu.sh
# - setup-starship.sh
# - setup-docker.sh
# - setup-haskell.sh
# - setup-elixir.sh
# - setup-rust.sh
# - setup-nodejs.sh
# - setup-emacs.sh
# - setup-shellcheck.sh
# - setup-c.sh

# TODO: install latest version of language && language server && utilities for
# - Elixir
# - JavaScript/TypeScript
# - Rust
# TODO: add to switch-theme a light theme that works
# TODO: Firefox minimal theme?
# TODO: Firefox theme customization with the same colors in switch-theme?
# TODO: remove chunkly things and references
# TODO: how to have non intrusive notifications in i3?
# TODO: emoji https://dev.to/darksmile92/get-emojis-working-on-arch-linux-with-noto-fonts-emoji-2a9

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

# echo "Update package repositories..."
sudo apt update

# echo "Install basic packages..."
sudo apt install -y \
     apt-transport-https \
     aspell-it \
     autoconf \
     autocutsel \
     binutils \
     bison \
     build-essential \
     ca-certificates \
     cmake \
     compton \
     coreutils \
     curl \
     dbus-x11 \
     dunst \
     entr \
     flex \
     fop \
     g++-8 \
     gcc-multilib \
     git \
     gnupg-agent \
     gnutls-bin \
     hsetroot \
     htop \
     i3 \
     iitalian \
     jq \
     libcanberra-gtk3-module \
     libfixposix-dev \
     libfixposix3 \
     libgccjit-10-dev \
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
     libmagic-dev \
     libmagickcore-dev \
     libmagickwand-dev \
     libmpc-dev \
     libmpfr-dev \
     libncurses-dev \
     libncurses5-dev \
     libnss3-tools \
     libpng-dev \
     libpoppler-glib-dev \
     libpoppler-private-dev \
     libreadline-dev \
     libssh-dev \
     libssl-dev \
     libtiff-dev \
     libtiff5-dev \
     libtinfo-dev \
     libtinfo5 \
     libtool \
     libwebkit2gtk-4.0-dev \
     libwxgtk3.0-gtk3-dev \
     libxaw7-dev \
     libxft-dev \
     libxml2-dev \
     libxml2-utils \
     libxpm-dev \
     libxt-dev \
     m4 \
     make \
     pandoc \
     ripgrep \
     rlwrap \
     silversearcher-ag \
     software-properties-common \
     suckless-tools \
     texinfo \
     unclutter \
     unixodbc-dev \
     unzip \
     vim \
     xclip \
     xsltproc \
     zlib1g-dev \
     zsh

echo "Cloning Emacs repository, be patient..."
mkdir -p ~/src && cd ~/src || exit 1
[ ! -d emacs ] && git clone git://git.sv.gnu.org/emacs.git emacs
cd emacs-build || exit 1
# EMACS_RELEASE=$(git describe --abbrev=0 --tags)
# echo "Compile and install Emacs $EMACS_RELEASE be patient..."
# ./autogen.sh
# ./configure --prefix=/home/coder/opt/emacs \
#             --bindir=/home/coder/opt/emacs/bin \
#             --with-modules \
#             --with-cairo \
#             --with-imagemagick \
#             --with-x=yes \
#             --with-x-toolkit=gtk3 \
#             --with-xwidgets \
#             --without-dbus \
#             --with-native-compilation \
#             --with-jpeg \
#             --with-png \
#             --with-rsvg \
#             --with-tiff \
#             --with-wide-int \
#             --with-xft \
#             --with-xml2 \
#             --with-xpm \
#             --with-json \
#             --with-mailutils
# make -j"$(nproc)" && make install
# make && make install

# Cask
# curl -fsSkL https://raw.github.com/cask/cask/master/go | python

# echo "Install ShellCheck..."
# mkdir -p ~/.local/bin
# if [ ! -f ~/.local/bin/pup ]; then
#   mkdir -p ~/tmp && cd ~/tmp || exit 1
#   wget --quiet "https://github.com/koalaman/shellcheck/releases/download/stable/shellcheck-stable.linux.x86_64.tar.xz"
#   tar --xz -xvf shellcheck-stable.linux.x86_64.tar.xz
#   cp shellcheck-stable/shellcheck ~/.local/bin/shellcheck
#   rm -rf shellcheck-stable
#   rm -f shellcheck-stable.linux.x86_64.tar.xz
# fi

# echo "Install pup..."
# mkdir -p ~/.local/bin
# if [ ! -f ~/.local/bin/pup ]; then
#   cd ~/tmp || exit 1
#   PUP_VERSION=0.4.0
#   wget -q https://github.com/ericchiang/pup/releases/download/v${PUP_VERSION}/pup_v${PUP_VERSION}_linux_amd64.zip
#   unzip pup_v${PUP_VERSION}_linux_amd64.zip
#   mv pup ~/.local/bin
#   rm -rf pup_v${PUP_VERSION}_linux_amd64.zip
# fi

# echo "Install MongoDB shell extensions..."
# if [ ! -s ~/.mongorc.js ]; then
#   curl -sL https://raw.github.com/gabrielelana/mongodb-shell-extensions/master/released/mongorc.js > ~/.mongorc.js
# fi

echo "Install ASDF and related plugins..."
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
cd ~/.asdf && git checkout "$(git describe --abbrev=0 --tags)" && cd - || exit 1
~/.asdf/bin/asdf plugin add erlang
~/.asdf/bin/asdf plugin add elixir
~/.asdf/bin/asdf plugin add ocaml
~/.asdf/bin/asdf plugin add opam
~/.asdf/bin/asdf plugin add ruby
~/.asdf/bin/asdf plugin add php
~/.asdf/bin/asdf plugin add nodejs
~/.asdf/bin/asdf plugin add java
~/.asdf/bin/asdf plugin add sbt
~/.asdf/bin/asdf plugin add scala
~/.asdf/bin/asdf plugin add direnv
~/.asdf/bin/asdf plugin add kubectl
~/.asdf/bin/asdf plugin add kubectx
~/.asdf/bin/asdf plugin add kustomize
~/.asdf/bin/asdf plugin add kind
~/.asdf/bin/asdf plugin add mkcert
export PATH="~/.asdf/bin:$PATH"

echo "Install ASDF direnv Plugin..."
~/.asdf/bin/asdf install direnv latest
~/.asdf/bin/asdf global direnv "$(~/.asdf/bin/asdf list direnv | tail -1 | tr -d ' ')"

# echo "Install K8s Stuffs..."
# ~/.asdf/bin/asdf install kubectl latest
# ~/.asdf/bin/asdf global kubectl "$(~/.asdf/bin/asdf list kubectl | tail -1 | tr -d ' ')"
# ~/.asdf/bin/asdf install kubectx latest
# ~/.asdf/bin/asdf global kubectx "$(~/.asdf/bin/asdf list kubectx | tail -1 | tr -d ' ')"
# ~/.asdf/bin/asdf install kustomize latest
# ~/.asdf/bin/asdf global kustomize "$(~/.asdf/bin/asdf list kustomize | tail -1 | tr -d ' ')"
# ~/.asdf/bin/asdf install kind latest
# ~/.asdf/bin/asdf global kind "$(~/.asdf/bin/asdf list kind | tail -1 | tr -d ' ')"

# echo "Install NodeJS..."
# ~/.asdf/plugins/nodejs/bin/import-release-team-keyring
# ~/.asdf/bin/asdf install nodejs latest
# ~/.asdf/bin/asdf global nodejs "$(~/.asdf/bin/asdf list nodejs | tail -1 | tr -d ' ')"
# npm install -g yarn prettier jsonlint eslint typescript ts-node typescript-formatter dockerfile-language-server-nodejs http-server
# [[ (! -f /usr/bin/python) && (-f /usr/bin/python3) ]] && sudo ln -s /usr/bin/python3 /usr/bin/python
# npm install -g import-js --unsafe-per

# echo "Install Rust..."
# curl https://sh.rustup.rs -sSf | sh -s -- --no-modify-path -y

# echo "Install Haskell Stack..."
# curl -sSL https://get.haskellstack.org/ | sh -s - -f -d ~/.local/bin/
# To install haskell language server clone # https://github.com/haskell/haskell-language-server and follow README # instructions for stack projects
# Then install utilities and libraries
# NOTE: consider to add `--library-profiling` as option to `stack-install`
# $ stack install hlint
# $ stack install QuickCheck
# $ stack install HSpec
# $ stack install random
# $ stack install checkers


# echo "Install Starship..."
# curl -fsSL https://starship.rs/install.sh | bash

# echo "Install Dhall..."
# mkdir -p ~/tmp/download-dhall && (cd ~/tmp/download-dhall || exit 1)
# for f in $(curl --silent 'https://api.github.com/repos/dhall-lang/dhall-haskell/releases/latest' | jq '.assets[].browser_download_url' | grep linux | tr -d '"'); do
#   echo "> download $f..."
#   wget --quiet "$f"
#   echo "> extract $f..."
#   tar --extract --bzip2 --file "$(basename "$f")"
#   rm -rf "$(basename "$f")"
# done
# cp ./bin/* ~/.local/bin && cd ~ && rm -rf ~/tmp/download-dhall

# echo "Install and configure Docker..."
# curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
# echo "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
# sudo apt-get install apt-transport-https ca-certificates curl gnupg lsb-release -y
# sudo apt-get update
# sudo apt-get install docker-ce docker-ce-cli containerd.io -y
# sudo groupadd docker
# sudo usermod -aG docker $USER
# newgrp docker
# sudo systemctl restart docker
# docker run hello-world || echo "! failed to install Docker"

# # Shell testing harness
# echo "Install bats..." && (
#   cd ~/tmp
#   git clone https://github.com/sstephenson/bats.git
#   cd bats
#   chmod +x ./install.sh
#   sudo ./install.sh /usr/local
# )

# Install clangd
# wget -O - https://apt.llvm.org/llvm.sh > ~/tmp/install-clangd.sh
