# Be a good citizen 
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Keyboad mappings
[ -f ~/.xmodmap ] && xmodmap ~/.xmodmap

# Clipboard synchronization
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

# Background
xsetroot -solid "#1c1c1c"

# Start VirtualBox client to enable clipboard exchange with host and screen autoresize
exec VBoxClient-all &

# Start window manager
exec i3 -V > ~/.i3/log 2>&1
