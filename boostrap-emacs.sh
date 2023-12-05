#!/bin/bash

echo "Install dependencies..."
sudo apt update
sudo apt install -y \
     git \
     mesa-utils \
     autoconf \
     binutils \
     build-essential \
     coreutils  \
     texinfo \
     libgtk-3-dev  \
     libimage-magick-perl  \
     libmagickcore-6.q16-dev \
     sqlite3 \
     libsqlite3-dev  \
     libmagickwand-dev  \
     libwebkit2gtk-4.0-dev  \
     gcc-12 \
     libgccjit-12-dev  \
     gcc-11 \
     libgccjit-11-dev  \
     libxpm-dev  \
     libgif-dev \
     libpng-dev \
     libtiff-dev  \
     libgnutls28-dev \
     libjansson-dev  \
     libncurses-dev \
     libvterm-dev  \
     libssh-dev \
     libssl-dev

echo "Cloning Emacs repository, be patient..."
mkdir -p ~/src && cd ~/src || exit 1
[ ! -d emacs ] && git clone git://git.sv.gnu.org/emacs.git emacs
cd emacs || exit 1
EMACS_RELEASE=$(git describe --abbrev=0 --tags)
./autogen.sh
./configure --prefix=/home/coder/opt/$EMACS_RELEASE \
            --bindir=/home/coder/opt/$EMACS_RELEASE/bin \
            --with-json \
            --with-tree-sitter \
            --with-imagemagick \
            --with-x \
            --with-x-toolkit=gtk3 \
            --with-xwidgets \
            --without-dbus \
            --with-native-compilation=aot \
            --with-wide-int \
            --with-mailutils
make -j"$(nproc)" && make install

# ca-certificates curl gnupg

# docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
# direnv
# ripgrep
# rlwrap
# dunst unclutter i3
# jq
# gnupg-agent compton
# aspell-it vim
# direnv
# cmake
# clangd-12
# htop
# cmake pkg-config libfreetype6-dev libfontconfig1-dev libxcb-xfixes0-dev libxkbcommon-dev python3
# sdoc
# scdoc
# hsetroot
