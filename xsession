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

# Background (xsetroot doesn't work with compositors like compton)
hsetroot -solid "#303347"

# Start VMware client to enable clipboard exchange with host
if command -v vmware-user-suid-wrapper; then
   exec vmware-user-suid-wrapper &
fi

# Adjust DPIs
# * xrandr needs a shell to do its work
# * if we don't wait a little bit X is gonna crash
exec bash -c 'sleep 1 && xrandr --output Virtual1 --scale 1x1 --dpi 96 && sleep 2' &

# Start Compton
exec compton --config ~/.i3/compton.conf &

# Tell Xorg to consider this file as configuration
xrdb ~/.xresources

# Start window manager
exec i3 > ~/.i3/log 2>&1
