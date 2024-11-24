#!/bin/bash

# echo "Install basic packages..."
# sudo apt install -y \
#      apt-transport-https \
#      aspell-it \
#      autoconf \
#      autocutsel \
#      binutils \
#      bison \
#      build-essential \
#      ca-certificates \
#      cmake \
#      compton \
#      coreutils \
#      curl \
#      dbus-x11 \
#      dunst \
#      entr \
#      flex \
#      fop \
#      git \
#      gnupg-agent \
#      gnutls-bin \
#      hsetroot \
#      htop \
#      i3 \
#      iitalian \
#      jq \
#      m4 \
#      make \
#      pandoc \
#      ripgrep \
#      rlwrap \
#      texinfo \
#      unclutter \
#      unixodbc-dev \
#      unzip \
#      vim \
#      xclip \
#      xsltproc \
#      || true

# echo "Install Docker..."
# if [ ! -f /etc/apt/keyrings/docker.asc ]; then
#     sudo apt-get update
#     sudo apt-get install ca-certificates curl
#     sudo install -m 0755 -d /etc/apt/keyrings
#     sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
#     sudo chmod a+r /etc/apt/keyrings/docker.asc
#     echo \
#         "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
#         $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
#         sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
#     sudo apt-get update
# fi
# sudo apt install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

echo "Install ASDF and related plugins..."
# git clone https://github.com/asdf-vm/asdf.git ~/.asdf
# cd ~/.asdf && git checkout "$(git describe --abbrev=0 --tags)" && cd - || exit 1
~/.asdf/bin/asdf plugin-add actionlint
~/.asdf/bin/asdf plugin-add awscli
~/.asdf/bin/asdf plugin-add aws-nuke
~/.asdf/bin/asdf plugin-add cue
~/.asdf/bin/asdf plugin-add direnv
~/.asdf/bin/asdf plugin-add elixir
~/.asdf/bin/asdf plugin-add erlang
~/.asdf/bin/asdf plugin-add golang
~/.asdf/bin/asdf plugin-add golangci-lint
~/.asdf/bin/asdf plugin-add hadolint
~/.asdf/bin/asdf plugin-add hurl
~/.asdf/bin/asdf plugin-add java
~/.asdf/bin/asdf plugin-add kind
~/.asdf/bin/asdf plugin-add kubectl
~/.asdf/bin/asdf plugin-add kubectx
~/.asdf/bin/asdf plugin-add kustomize
~/.asdf/bin/asdf plugin-add mkcert
~/.asdf/bin/asdf plugin-add nodejs
~/.asdf/bin/asdf plugin-add ocaml
~/.asdf/bin/asdf plugin-add opa
~/.asdf/bin/asdf plugin-add opam
~/.asdf/bin/asdf plugin-add php
~/.asdf/bin/asdf plugin-add python
~/.asdf/bin/asdf plugin-add regal
~/.asdf/bin/asdf plugin-add ruby
~/.asdf/bin/asdf plugin-add sbcl
~/.asdf/bin/asdf plugin-add sbt
~/.asdf/bin/asdf plugin-add scala
~/.asdf/bin/asdf plugin-add shellcheck
~/.asdf/bin/asdf plugin-add terraform
~/.asdf/bin/asdf plugin-add terraform-ls
~/.asdf/bin/asdf plugin-add terragrunt
~/.asdf/bin/asdf plugin-add tflint
~/.asdf/bin/asdf plugin-add tfsec
~/.asdf/bin/asdf plugin-add yq
export PATH="$HOME/.asdf/bin:$PATH"

# echo "Install ASDF direnv Plugin..."
# ~/.asdf/bin/asdf install direnv latest
# ~/.asdf/bin/asdf global direnv "$(~/.asdf/bin/asdf list direnv | tail -1 | tr -d ' ')"

echo "Install Terraform..."
~/.asdf/bin/asdf install terraform latest
~/.asdf/bin/asdf install terraform-ls latest
~/.asdf/bin/asdf install terragrunt latest
~/.asdf/bin/asdf install tflint latest
~/.asdf/bin/asdf install tfsec latest
~/.asdf/bin/asdf global terraform latest
~/.asdf/bin/asdf global terraform-ls latest
~/.asdf/bin/asdf global terragrunt latest
~/.asdf/bin/asdf global tflint latest
~/.asdf/bin/asdf global tfsec latest

echo "Install AWS..."
~/.asdf/bin/asdf install awscli latest
~/.asdf/bin/asdf global awscli latest
~/.asdf/bin/asdf install aws-nuke latest
~/.asdf/bin/asdf global aws-nuke latest
# TODO: install session manager plugin to be able to open a shell on instances/containers
# https://docs.aws.amazon.com/systems-manager/latest/userguide/install-plugin-debian-and-ubuntu.html

echo "Install Docker & K8s Stuffs..."
~/.asdf/bin/asdf install kubectl latest
~/.asdf/bin/asdf global kubectl latest
~/.asdf/bin/asdf install kubectx latest
~/.asdf/bin/asdf global kubectx latest
~/.asdf/bin/asdf install kustomize latest
~/.asdf/bin/asdf global kustomize latest
~/.asdf/bin/asdf install kind latest
~/.asdf/bin/asdf global kind latest
~/.asdf/bin/asdf install hadolint latest
~/.asdf/bin/asdf global hadolint latest

echo "Install NodeJS..."
~/.asdf/bin/asdf install nodejs latest
~/.asdf/bin/asdf global nodejs latest
~/.asdf/bin/asdf install hurl latest
~/.asdf/bin/asdf global hurl latest
npm config set init-author-name "Gabriele Lana"
npm config set init-author-email "gabriele.lana@gmail.com"
npm config set init-author-url "http://github.com/gabrielelana"
# Run install-global-npm-packages

echo "Install Golang..."
~/.asdf/bin/asdf install golang latest
~/.asdf/bin/asdf global golang latest
~/.asdf/bin/asdf install golangci-lint latest
~/.asdf/bin/asdf global golangci-lint latest
# go install golang.org/x/tools/gopls@latest
# go install github.com/fatih/gomodifytags@latest
# go install golang.org/x/tools/cmd/goimports@latest
# go install honnef.co/go/tools/cmd/staticcheck@latest

echo "Installing OPA..."
~/.asdf/bin/asdf install opa latest
~/.asdf/bin/asdf global opa latest
~/.asdf/bin/asdf install regal latest
~/.asdf/bin/asdf global regal latest

echo "Intall shellcheck..."
~/.asdf/bin/asdf install shellcheck latest
~/.asdf/bin/asdf global shellcheck latest

# echo "Install Starship..."
# curl -sS https://starship.rs/install.sh | sh
