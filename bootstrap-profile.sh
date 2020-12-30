#!/bin/bash

cd "$(dirname "$0")" || exit 1

ROOT=$PWD

echo "Prepare..."
mkdir -p "$ROOT"/.dependencies

echo "Update dotfiles..."
git pull --rebase

echo "Update dependencies..."
for project in dotfiles-secrets awesome-terminal-fonts; do
  echo "Update ${project}..."
  if [ ! -d "$ROOT"/.dependencies/$project ]; then
    git clone git@github.com:gabrielelana/$project.git "$ROOT"/.dependencies/"$project"
  else
    cd "$ROOT"/.dependencies/$project || exit 1
    git pull --rebase
    cd "$ROOT" || exit 1
  fi
done

echo "Install executables..."
mkdir -p ~/bin
mkdir -p ~/.local/bin
for file in "$ROOT"/bin/*; do
  ln -sf "$file" ~/bin/"$(basename "$file")"
done

echo "Install configuration files..."
cp -f "$ROOT"/ackrc ~/.ackrc
cp -f "$ROOT"/rvmrc ~/.rvmrc
cp -f "$ROOT"/gitignore ~/.gitignore
cp -f "$ROOT"/.dependencies/dotfiles-secrets/gitconfig ~/.gitconfig
cp -rf "$ROOT"/i3 ~/.i3
cp -f "$ROOT"/dunstrc ~/.dunstrc
cp -f "$ROOT"/gtkrc-2.0 ~/.gtkrc-2.0
cp -f "$ROOT"/xsession ~/.xsession
cp -f "$ROOT"/xmodmap ~/.xmodmap
cp -f "$ROOT"/xresources ~/.xresources
ln -sf "$ROOT"/docker ~/.docker
mkdir -p ~/.config/direnv && ln -sf $"ROOT"/direnvrc ~/.config/direnv/direnvrc

echo "Configure shell..."
ln -sf "$ROOT"/profile ~/.profile
ln -sf "$ROOT"/bashrc ~/.bashrc
if [ ! -f ~/.bashrc.localhost ]; then
  echo "#!/bin/bash" > ~/.bashrc.localhost
fi
mkdir -p ~/.config && ln -sf "$ROOT"/starship.toml ~/.config/starship.toml

echo "Setup fonts..."
mkdir -p ~/.fonts
cp -f "$ROOT"/.dependencies/awesome-terminal-fonts/build/* ~/.fonts
cp -f "$ROOT"/.dependencies/dotfiles-secrets/fonts/*.ttf ~/.fonts
mkdir -p ~/.config/fontconfig/conf.d && \
  cp -f "$ROOT"/.dependencies/awesome-terminal-fonts/config/* ~/.config/fontconfig/conf.d
mkdir -p ~/.config/fontconfig && \
  cp -f "$ROOT"/fonts.conf ~/.config/fontconfig/fonts.conf
fc-cache -fv ~/.fonts

echo "Setup identity..."
mkdir -p ~/.ssh
cp -f "$ROOT"/.dependencies/dotfiles-secrets/ssh/* ~/.ssh
cp -f "$ROOT"/.dependencies/dotfiles-secrets/netrc ~/.netrc
cp -f "$ROOT"/.dependencies/dotfiles-secrets/npmrc ~/.npmrc
chmod 0400 ~/.ssh/*
chmod 0400 ~/.netrc
chmod 0400 ~/.npmrc

echo "Setup Vim..."
cp -f "$ROOT"/vimrc ~/.vimrc

echo "Setup Emacs..."
ln -sf "$ROOT"/emacs.d ~/.emacs.d

echo "Setup Haskell (stack)..."
cp -rf "$ROOT"/stack ~/.stack

echo "Configure gnome terminal..."
if [ -x "$(command -v dconf)" ]; then
  dconf reset -f "/org/gnome/terminal/"
  dconf load "/org/gnome/terminal/" < "$ROOT"/gnome-terminal.ini
fi

echo "Add an XSession entry for GDM3..."
sudo cat <<EOF | sudo tee /usr/share/xsessions/xsession.desktop
[Desktop Entry]
Name=XSession
Comment=This session uses the custom xsession file
Exec=/etc/X11/Xsession
Type=Application
DesktopNames=GNOME-Flashback;GNOME;
X-Ubuntu-Gettext-Domain=gnome-flashback
EOF

echo "Install default theme and restart..."
~/bin/switch-theme -t subatomic
