#!/bin/zsh

cd "$(dirname "$0")"

declare -A flags
flags[without-x]=0          # do not configure things related to x
flags[without-identity]=0   # do not configure things related to identity

for option in $@; do
  flags[${option/--/}]=1
done

echo "update dotfiles..."
git pull

echo "install configuration files..."
rm -rf ~/bin && cp -rf $PWD/bin ~/bin
chmod +x ~/bin
cp -f $PWD/ackrc ~/.ackrc
cp -f $PWD/rvmrc ~/.rvmrc
cp -f $PWD/gitignore ~/.gitignore
cp -f $PWD/gitconfig ~/.gitconfig
if [ ${flags[without-x]} -eq 0 ]; then
  rm -rf ~/.i3 && cp -rf $PWD/i3 ~/.i3
  cp -f $PWD/dunstrc ~/.dunstrc
  cp -f $PWD/gtkrc-2.0 ~/.gtkrc-2.0
  cp -f $PWD/xinitrc ~/.xsession
  cp -f $PWD/xmodmap ~/.xmodmap
  cp -f $PWD/xresources ~/.xresources
  cp -f $PWD/vimperatorrc ~/.vimperatorrc
fi

echo "update dependencies..."
mkdir -p $PWD/.dependencies
for project in dotfiles-secrets awesome-terminal-fonts; do
  echo "update ${project}..."
  if [ ! -d $PWD/.dependencies/$project ]; then
    git clone git@github.com:gabrielelana/$project.git $PWD/.dependencies/$project
  else
    cd $PWD/.dependencies/$project
    git pull
    cd $OLDPWD
  fi
done

echo "setup fonts..."
mkdir -p ~/.fonts
cp -f $PWD/.dependencies/awesome-terminal-fonts/build/* ~/.fonts
cp -f $PWD/.dependencies/dotfiles-secrets/fonts/*.ttf ~/.fonts
mkdir -p ~/.config/fontconfig/conf.d
cp -f $PWD/.dependencies/awesome-terminal-fonts/config/* ~/.config/fontconfig/conf.d
if [ ${flags[without-x]} -eq 0 ]; then
  cp -f $PWD/fonts.conf ~/.config/fontconfig/fonts.conf
  fc-cache -fv ~/.fonts
fi

if [ ${flags[without-identity]} -eq 0 ]; then
  echo "setup identity..."
  mkdir -p ~/.ssh
  cp -f $PWD/.dependencies/dotfiles-secrets/ssh/* ~/.ssh
  cp -f $PWD/.dependencies/dotfiles-secrets/netrc ~/.netrc
  chmod 0400 ~/.ssh/*
  if [ ! -f ~/.npmrc ]; then
    cp -f $PWD/.dependencies/dotfiles-secrets/npmrc ~/.npmrc
  fi
fi

echo "setup vim..."
rm -rf ~/.vim && cp -rf $PWD/vim ~/.vim
cp -f $PWD/vimrc ~/.vimrc
if [ ! -d ~/.vim/bundle/vundle ]; then
  git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
fi
# vim +PluginInstall! +qall &>/dev/null

echo "setup emacs..."
ln -sf $PWD/emacs.d ~/.emacs.d

echo "setup zsh..."
if [ ! -d ~/.oh-my-zsh ]; then
  git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi
export ZSH=$HOME/.oh-my-zsh
cp -rfu $PWD/zsh/* $HOME/.oh-my-zsh/custom
mkdir -p $HOME/.zsh_functions
for file in $PWD/zshrc*; do
  ln -sf $file "$HOME/.`basename $file`"
  if [[ $file =~ "localhost" ]]; then
    cp -f $file "$HOME/.`basename $file`"
  fi
done
zsh $HOME/.zshrc
zsh $ZSH/tools/upgrade.sh
if [ ! -f $HOME/.zshrc.localhost ]; then
  echo "# -*- mode: Shell-script; sh-basic-offset: 2; -*-" > $HOME/.zshrc.localhost
fi

if [ ${flags[without-x]} -eq 0 ]; then
  echo "setup chunkly..."
  mkdir -p ~/.chunkly
  cp -f $PWD/chunkly.vimrc ~/.chunkly/.vimrc
  rsync -r $PWD/.dependencies/dotfiles-secrets/chunkly/ ~/.chunkly
fi

if [ ${flags[without-x]} -eq 0 ] && [ -n "$DISPLAY" ]; then
  echo "configure gnome terminal..."
  dconf reset -f "/org/gnome/terminal/"
  cat $PWD/gnome-terminal.ini | dconf load "/org/gnome/terminal/"
fi
