#!/bin/bash

cd "$(dirname "$0")" || (echo "Run this script in its directory"; exit 1)

ROOT=$PWD

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
mkdir -p ~/.local/bin
for file in "$ROOT"/bin/*; do
  ln -sf "$file" ~/.local/bin/"$(basename "$file")"
done

echo "Install configuration files..."
ln -sf "$ROOT"/gitignore ~/.gitignore
ln -sf "$ROOT"/.dependencies/dotfiles-secrets/gitconfig ~/.gitconfig
ln -sf "$ROOT"/i3 ~/.i3
ln -sf "$ROOT"/dunstrc ~/.dunstrc
ln -sf "$ROOT"/xsession ~/.xsession
ln -sf "$ROOT"/xmodmap ~/.xmodmap
ln -sf "$ROOT"/xresources ~/.xresources
ln -sf "$ROOT"/asdfrc ~/.asdfrc
# TODO: check if still needed
# ln -sf "$ROOT"/aspell.en.prepl ~/.aspell.en.prepl
# ln -sf "$ROOT"/aspell.en.pws ~/.aspell.en.pws
mkdir -p ~/.config/direnv && ln -sf "$ROOT"/direnvrc ~/.config/direnv/direnvrc

echo "Configure Docker..."
mkdir -p ~/.docker
if [ -f ~/.docker/config.json ]; then
    cat ~/.docker/config.json "$ROOT"/docker-config.json | jq -s add > ~/.docker/config.merged
    mv ~/.docker/config.merged ~/.docker/config.json
else
    cp "$ROOT"/docker-config.json ~/.docker/config.json
fi

echo "Configure shell..."
ln -sf "$ROOT"/profile ~/.profile
ln -sf "$ROOT"/bashrc ~/.bashrc
if [ ! -f ~/.bashrc.localhost ]; then
  echo "#!/bin/bash" > ~/.bashrc.localhost
fi

echo "Configure starship..."
mkdir -p ~/.config && ln -sf "$ROOT"/starship.toml ~/.config/starship.toml

echo "Setup fonts..."
mkdir -p ~/.fonts
cp -f "$ROOT"/.dependencies/awesome-terminal-fonts/build/* ~/.fonts
cp -f "$ROOT"/.dependencies/dotfiles-secrets/fonts/*.ttf ~/.fonts
mkdir -p ~/.config/fontconfig/conf.d && \
  cp -f "$ROOT"/.dependencies/awesome-terminal-fonts/config/* ~/.config/fontconfig/conf.d
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

echo "Consider ~/.xsession file with GDM3..."
# Otherwise the Gnome login manager will not consider the ~/.xsession file
sudo cat <<EOF | sudo tee /usr/share/xsessions/xsession.desktop
[Desktop Entry]
Name=XSession
Comment=This session uses the custom xsession file
Exec=/etc/X11/Xsession
Type=Application
DesktopNames=GNOME-Flashback;GNOME;
X-Ubuntu-Gettext-Domain=gnome-flashback
EOF
