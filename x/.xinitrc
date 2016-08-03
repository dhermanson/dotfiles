xrdb -load .Xresources
feh --bg-scale ~/background.jpg &
compton -b &

#setxkbmap -option '' -option 'ctrl:swapcaps'

xmodmap ~/.Xmodmap &

if [ -f '/home/derick/.xrandr.conf' ]
then
  zsh /home/derick/.xrandr.conf
fi

exec xmonad
