#!/bin/zsh

cd "$(dirname "$0")"

if [ -d slim-themes/linen ]; then
  cp -r $PWD/slim-themes/* /usr/share/slim/themes/
  sed -i 's/^current_theme.*$/current_theme\t\tlinen/' /etc/slim.conf
fi
