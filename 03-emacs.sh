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
     libwebkit2gtk-4.1-dev  \
     gcc-14 \
     libgccjit-14-dev  \
     libxpm-dev  \
     libgif-dev \
     libpng-dev \
     libtiff-dev  \
     libgnutls28-dev \
     libjansson-dev  \
     libtree-sitter-dev  \
     libncurses-dev \
     libvterm-dev  \
     libssh-dev \
     libssl-dev

echo "Cloning Emacs repository, be patient..."
mkdir -p ~/src && cd ~/src || exit 1
[ ! -d emacs ] && git clone git://git.sv.gnu.org/emacs.git emacs
cd emacs || exit 1

EMACS_RELEASE=$(git describe --abbrev=0 --tags)
echo "Compile Emacs ${EMACS_RELEASE}..."
git checkout $EMACS_RELEASE
make clean && git clean -xfd
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

echo "Compile Emacs latest..."
git checkout master
make clean && git clean -xfd
./autogen.sh
./configure --prefix=/home/coder/opt/emacs-latest \
            --bindir=/home/coder/opt/emacs-latest/bin \
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
