#!/bin/bash

cd "$(dirname "$0")"

echo "update dotfiles..."
git pull

echo "update dependencies..."
mkdir -p $PWD/.dependencies
for project in dotfiles-secrets awesome-terminal-fonts; do
  echo "update ${project}..."
  if [ ! -d $PWD/.dependencies/$project ]; then
    git clone https://github.com/gabrielelana/$project $PWD/.dependencies/$project
  else
    cd $PWD/.dependencies/$project
    git pull
    cd $OLDPWD
  fi
done

echo "setup fonts..."
mkdir -p ~/.fonts
cp -f $PWD/.dependencies/awesome-terminal-fonts/fonts/*.ttf ~/.fonts
cp -f $PWD/.dependencies/awesome-terminal-fonts/maps/*.sh ~/.fonts
cp -f $PWD/.dependencies/dotfiles-secrets/fonts/*.ttf ~/.fonts
mkdir -p ~/.config/fontconfig/conf.d
cp -f $PWD/.dependencies/awesome-terminal-fonts/config/* ~/.config/fontconfig/conf.d
fc-cache -fv ~/.fonts

echo "setup ssh..."
mkdir -p ~/.ssh
cp -f $PWD/.dependencies/dotfiles-secrets/ssh ~/.ssh
cp -f $PWD/.dependencies/dotfiles-secrets/netrc ~/.netrc





ln -sfT $PWD/ackrc ~/.ackrc
ln -sfT $PWD/gitconfig ~/.gitconfig

[[ ! -h ~/.bin ]] && rm -rf ~/.bin
ln -sfT $PWD/bin ~/.bin

[[ ! -h ~/.i3 ]] && rm -rf ~/.i3
ln -sfT $PWD/i3 ~/.i3

ln -sfT $PWD/dunstrc ~/.dunstrc
ln -sfT $PWD/gtkrc-2.0 ~/.gtkrc-2.0

ln -sfT $PWD/xinitrc ~/.xinitrc
ln -sfT $PWD/xinitrc ~/.xsession
ln -sfT $PWD/xinitrc ~/.xprofile
ln -sfT $PWD/xdefaults ~/.xdefaults
ln -sfT $PWD/xmodmap ~/.xmodmap

[[ ! -h ~/.vim ]] && rm -rf ~/.vim
ln -sfT $PWD/vim ~/.vim
ln -sfT $PWD/vimrc ~/.vimrc
if [ ! -d ~/.vim/bundle/vundle ]; then
  git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
fi
vim +BundleInstall! +qall > /dev/null 2>&1

if [ ! -d ~/.oh-my-zsh ]; then
  git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

for file in `ls $PWD/zsh`; do
  ln -sfT $PWD/zsh/$file ~/.oh-my-zsh/custom/$file
done

ln -sfT $PWD/zshrc ~/.zshrc
export ZSH=~/.oh-my-zsh
if which zsh > /dev/null; then
  zsh ~/.zshrc
  zsh $ZSH/tools/upgrade.sh
fi
