#!/bin/bash

echo "Install basic packages..."
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
     git \
     gnupg-agent \
     gnutls-bin \
     hsetroot \
     htop \
     i3 \
     iitalian \
     jq \
     m4 \
     make \
     pandoc \
     ripgrep \
     rlwrap \
     texinfo \
     unclutter \
     unixodbc-dev \
     unzip \
     vim \
     xclip \
     xsltproc \
     || true

echo "Install Docker..."
if [ ! -f /etc/apt/keyrings/docker.asc ]; then
    sudo apt-get update
    sudo apt-get install ca-certificates curl
    sudo install -m 0755 -d /etc/apt/keyrings
    sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
    sudo chmod a+r /etc/apt/keyrings/docker.asc
    echo \
        "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
        $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
        sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
    sudo apt-get update
fi
sudo apt install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

echo "Install ASDF and related plugins..."
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
cd ~/.asdf && git checkout "$(git describe --abbrev=0 --tags)" && cd - || exit 1
~/.asdf/bin/asdf plugin-add direnv
~/.asdf/bin/asdf plugin-add elixir
~/.asdf/bin/asdf plugin-add erlang
~/.asdf/bin/asdf plugin-add java
~/.asdf/bin/asdf plugin-add hadolint
~/.asdf/bin/asdf plugin-add kind
~/.asdf/bin/asdf plugin-add kubectl
~/.asdf/bin/asdf plugin-add kubectx
~/.asdf/bin/asdf plugin-add kustomize
~/.asdf/bin/asdf plugin-add mkcert
~/.asdf/bin/asdf plugin-add nodejs
~/.asdf/bin/asdf plugin-add ocaml
~/.asdf/bin/asdf plugin-add opam
~/.asdf/bin/asdf plugin-add php
~/.asdf/bin/asdf plugin-add python
~/.asdf/bin/asdf plugin-add ruby
~/.asdf/bin/asdf plugin-add sbcl
~/.asdf/bin/asdf plugin-add sbt
~/.asdf/bin/asdf plugin-add scala
~/.asdf/bin/asdf plugin-add shellcheck
export PATH="$HOME/.asdf/bin:$PATH"

echo "Install ASDF direnv Plugin..."
~/.asdf/bin/asdf install direnv latest
~/.asdf/bin/asdf global direnv "$(~/.asdf/bin/asdf list direnv | tail -1 | tr -d ' ')"

echo "Install K8s Stuffs..."
~/.asdf/bin/asdf install kubectl latest
~/.asdf/bin/asdf global kubectl latest
~/.asdf/bin/asdf install kubectx latest
~/.asdf/bin/asdf global kubectx latest
~/.asdf/bin/asdf install kustomize latest
~/.asdf/bin/asdf global kustomize latest
~/.asdf/bin/asdf install kind latest
~/.asdf/bin/asdf global kind latest

echo "Install NodeJS..."
~/.asdf/bin/asdf install nodejs latest
~/.asdf/bin/asdf global nodejs latest
npm config set init-author-name "Gabriele Lana"
npm config set init-author-email "gabriele.lana@gmail.com"
npm config set init-author-url "http://github.com/gabrielelana"

echo "Intall shellcheck..."
~/.asdf/bin/asdf install shellcheck latest
~/.asdf/bin/asdf global shellcheck latest

echo "Install Starship..."
curl -sS https://starship.rs/install.sh | sh
