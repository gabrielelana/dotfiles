#!/bin/bash

echo "Installing rustup..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --profile complete --no-modify-path -y

echo "Install Alacritty..."
sudo apt install -y cmake pkg-config libfreetype6-dev libfontconfig1-dev libxcb-xfixes0-dev libxkbcommon-dev python3
cd ~/opt && \
    git clone https://github.com/alacritty/alacritty.git && \
    cd alacritty && \
    cargo build --release && \
    cp target/release/alacritty ~/.local/bin && \
    ln -sf ~/.dotfiles/alacritty.toml ~/.alacritty.toml
