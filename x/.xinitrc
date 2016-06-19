xrdb -load .Xresources
feh --bg-scale ~/background.jpg &

setxkbmap -option '' -option 'ctrl:swapcaps'

exec xmonad
