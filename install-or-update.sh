#!/bin/zsh

cd "$(dirname "$0")"

ROOT=$PWD
DEFAULT_THEME=subatomic

declare -A flags
flags[without-x]=0          # do not configure things related to x
flags[without-identity]=0   # do not configure things related to identity

for option in $@; do
  flags[${option/--/}]=1
done

echo "prepare..."
mkdir -p $ROOT/.dependencies

echo "update dotfiles..."
git pull

if ! [ -d $ROOT/.dependencies/st ]; then
  echo "● install suckless terminal..."
  mkdir $ROOT/.dependencies/st
  cd $ROOT/.dependencies/st
  wget --quiet https://dl.suckless.org/st/st-0.8.2.tar.gz
  tar -xzf st-0.8.2.tar.gz
  cd st-0.8.2
  patch < $ROOT/st/st-clipboard-0.8.2.diff
  patch < $ROOT/st/st-scrollback-0.8.2.diff
  patch < $ROOT/st/st-pragmata-0.8.2.diff
  sudo make clean install
  cd $ROOT
fi

exit 0

echo "install configuration files..."
rm -rf ~/bin && cp -rf $ROOT/bin ~/bin
chmod +x ~/bin
cp -f $ROOT/ackrc ~/.ackrc
cp -f $ROOT/rvmrc ~/.rvmrc
cp -f $ROOT/gitignore ~/.gitignore
cp -f $ROOT/gitconfig ~/.gitconfig
if [ ${flags[without-x]} -eq 0 ]; then
  rm -rf ~/.i3 && cp -rf $ROOT/i3 ~/.i3
  cp -f $ROOT/dunstrc ~/.dunstrc
  cp -f $ROOT/gtkrc-2.0 ~/.gtkrc-2.0
  cp -f $ROOT/xsession ~/.xsession
  cp -f $ROOT/xmodmap ~/.xmodmap
  cp -f $ROOT/xresources ~/.xresources
  # select desktop theme
  ~/bin/switch-theme $DEFAULT_THEME
  # select cursor theme
  [ -f /etc/X11/cursors/whiteglass.theme ] && \
    mkdir -p $ROOT/.icons/default && \
    ln -s /etc/X11/cursors/whiteglass.theme $ROOT/.icons/default/index.theme
fi

echo "update dependencies..."
for project in dotfiles-secrets awesome-terminal-fonts; do
  echo "update ${project}..."
  if [ ! -d $ROOT/.dependencies/$project ]; then
    git clone git@github.com:gabrielelana/$project.git $ROOT/.dependencies/$project
  else
    cd $ROOT/.dependencies/$project
    git pull
    cd $ROOT
  fi
done

echo "setup fonts..."
mkdir -p ~/.fonts
cp -f $ROOT/.dependencies/awesome-terminal-fonts/build/* ~/.fonts
cp -f $ROOT/.dependencies/dotfiles-secrets/fonts/*.ttf ~/.fonts
mkdir -p ~/.config/fontconfig/conf.d
cp -f $ROOT/.dependencies/awesome-terminal-fonts/config/* ~/.config/fontconfig/conf.d
if [ ${flags[without-x]} -eq 0 ]; then
  cp -f $ROOT/fonts.conf ~/.config/fontconfig/fonts.conf
  fc-cache -fv ~/.fonts
fi

if [ ${flags[without-identity]} -eq 0 ]; then
  echo "setup identity..."
  mkdir -p ~/.ssh
  cp -f $ROOT/.dependencies/dotfiles-secrets/ssh/* ~/.ssh
  cp -f $ROOT/.dependencies/dotfiles-secrets/netrc ~/.netrc
  chmod 0600 ~/.ssh/*
  if [ ! -f ~/.npmrc ]; then
    cp -f $ROOT/.dependencies/dotfiles-secrets/npmrc ~/.npmrc
  fi
fi

echo "setup vim..."
cp -f $ROOT/vimrc ~/.vimrc

echo "setup emacs..."
ln -sf $ROOT/emacs.d ~/.emacs.d

echo "setup haskell (stack)..."
mkdir -p ~/.stack
cp -rf $ROOT/stack ~/.stack

echo "setup zsh..."
if [ ! -d ~/.oh-my-zsh ]; then
  git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi
export ZSH=~/.oh-my-zsh
cp -rfu $ROOT/zsh/* ~/.oh-my-zsh/custom
mkdir -p ~/.zsh_functions
for file in $ROOT/zshrc*; do
  ln -sf $file "$HOME/.`basename $file`"
  if [[ $file =~ "localhost" ]]; then
    cp -f $file "$HOME/.`basename $file`"
  fi
done
zsh ~/.zshrc
zsh $ZSH/tools/upgrade.sh
if [ ! -f ~/.zshrc.localhost ]; then
  echo "# -*- mode: shell-script; sh-basic-offset: 2; -*-" > ~/.zshrc.localhost
fi

if [ ${flags[without-x]} -eq 0 ]; then
  echo "setup chunkly..."
  mkdir -p ~/.chunkly
  cp -f $ROOT/chunkly.vimrc ~/.chunkly/.vimrc
  rsync -r $ROOT/.dependencies/dotfiles-secrets/chunkly/ ~/.chunkly
fi

if [ ${flags[without-x]} -eq 0 ] && [ -n "$DISPLAY" ]; then
  echo "configure gnome terminal..."
  dconf reset -f "/org/gnome/terminal/"
  cat $ROOT/gnome-terminal.ini | dconf load "/org/gnome/terminal/"
fi

echo "install utilities..."

if [ ! -s ~/.mongorc.js ]; then
  echo "● install mongodb-shell-extensions..."
  curl -sL https://raw.github.com/gabrielelana/mongodb-shell-extensions/master/released/mongorc.js > ~/.mongorc.js
fi

if [ ! -f ~/bin/pup ]; then
  echo "● install pup..."
  PUP_VERSION=0.4.0
  wget -q https://github.com/ericchiang/pup/releases/download/v${PUP_VERSION}/pup_v${PUP_VERSION}_linux_amd64.zip
  unzip pup_v${PUP_VERSION}_linux_amd64.zip
  mv pup ~/bin
  rm -rf pup_v${PUP_VERSION}_linux_amd64.zip
fi
