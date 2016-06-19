require 'pathname'

abs_path = File.absolute_path(__FILE__)
dir = File.dirname(abs_path)

x_dir = Pathname.new(dir) + "../../x"
x_init_rc = "#{x_dir}/.xinitrc"
x_resources = "#{x_dir}/.Xresources"
xmonad_dir = "#{x_dir}/xmonad"

File.symlink x_init_rc, "#{Dir.home}/.xinitrc"
File.symlink x_resources, "#{Dir.home}/.Xresources"
File.symlink xmonad_dir, "#{Dir.home}/.xmonad"
