require 'fileutils'

unless Dir.exists?("#{Dir.home}/bin")
  puts "creating bin folder in home directory"
  FileUtils.mkdir "#{Dir.home}/bin"
end

unless Dir.exists?("#{Dir.home}/.repos/github/dhermanson/dotfiles")
  puts "cloning my dotfiles repo"
  system "git clone --recursive https://github.com/dhermanson/dotfiles #{Dir.home}/.repos/github/dhermanson/dotfiles"
end

unless File.symlink?("#{Dir.home}/.vimrc")
  system "ln -s #{Dir.home}/.repos/github/dhermanson/dotfiles/vim/.vimrc #{Dir.home}/.vimrc"
end

unless File.symlink?("#{Dir.home}/.vim")
  system "ln -s #{Dir.home}/.repos/github/dhermanson/dotfiles/vim/.vim #{Dir.home}/.vim"
end

unless File.symlink?("#{Dir.home}/.tmux.conf")
  puts "symlinking tmux conf"
  system "ln -s #{Dir.home}/.repos/github/dhermanson/dotfiles/tmux/.tmux.conf #{Dir.home}/.tmux.conf"
end

unless File.symlink?("#{Dir.home}/bin/create-php-ctags.sh")
  puts "symlinking create-php-ctags.sh"
  system "ln -s #{Dir.home}/.repos/github/dhermanson/dotfiles/bash/create-php-ctags.sh #{Dir.home}/bin/create-php-ctags.sh"
end

unless File.symlink?("#{Dir.home}/bin/create-php-vendor-tags.sh")
  puts "symlinking create-php-vendor-tags.sh"
  system "ln -s #{Dir.home}/.repos/github/dhermanson/dotfiles/bash/create-php-vendor-tags.sh #{Dir.home}/bin/create-php-vendor-tags.sh"
end

unless Dir.exists?("#{Dir.home}/.config/nvim")
  puts "setting up nvim directory"
  FileUtils.mkdir_p "#{Dir.home}/.config/nvim"
  system "ln -s #{Dir.home}/.vimrc #{Dir.home}/.config/nvim/init.vim"
  system "ln -s #{Dir.home}/.vim/UltiSnips #{Dir.home}/.config/nvim/UltiSnips"
  system "ln -s #{Dir.home}/.vim/bundle #{Dir.home}/.config/nvim/bundle"
  system "ln -s #{Dir.home}/.vim/plugin #{Dir.home}/.config/nvim/plugin"
  system "ln -s #{Dir.home}/.vim/syntax #{Dir.home}/.config/nvim/syntax"
end

unless File.symlink?("#{Dir.home}/.ctags")
  puts "symlinking .ctags"
  system "ln -s #{Dir.home}/.repos/github/dhermanson/dotfiles/ctags/.ctags #{Dir.home}/.ctags"
end
