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
xsetroot -solid "#2E3440"

# Start VirtualBox client to enable clipboard exchange with host and screen autoresize
if command -v VBoxClient >/dev/null; then
   exec sudo VBoxClient --clipboard &
fi

# Start VMware client to enable clipboard exchange with host
if command -v vmware-user-suid-wrapper; then
   exec vmware-user-suid-wrapper &
fi

# Tell XOrg to consider this file as configuration
xrdb ~/.xresources

# Adjust DPIs
# * xrandr needs a shell to do its work
# * if we don't wait a little bit X is gonna crash
exec bash -c 'sleep 1 && xrandr --output Virtual1 --dpi 192' &

# Wait for xrandr to do its job
sleep 2

# Start window manager
exec i3 > ~/.i3/log 2>&1