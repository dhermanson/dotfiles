# packages to install on the system
ubuntu_packages = [
  # tools
  "exuberant-ctags",
  "git",
  "silversearcher-ag",
  "tmux",
  "zsh",
  "vagrant",

  # editors
  "emacs",
  "neovim",
  "vim-gtk3",

  # databases
  "mysql-server",
  "mysql-workbench",
  "redis-server",
  
  # erlang / elixir
  "esl-erlang",
  "elixir",

  # php
  "php7.0",
  "php7.0-common",
  "php7.0-mbstring",
  "php7.0-mcrypt",
  "php7.0-mysql",
  "php7.0-xml",
  "php7.0-zip",
  "hhvm",

  # python
  "python-dev",
  "python-pip",
  "python3-dev",
  "python3-pip",

]

ubuntu_packages.each do |package|
  # TODO: check if package is installed first
  if `dpkg -s #{package} | grep Status:`.match('install ok installed')
    puts "#{package} already installed"
  else
    puts "installing #{package}"
    system "sudo apt-get install #{package} --assume-yes"
  end
end


pip3_packages = [
  "neovim"
]

pip3_packages.each do |package|
  unless system "pip3 list | grep #{package}"
    puts "runing pip3 install #{package}"
    system "pip3 install #{package}"
  end
end

#ruby_gems = [
  #'pry',
  #'pry-doc'
#]

#ruby_gems.each do |gem|
  #unless system "which #{gem}"
    #system "gem install #{gem}"
  #end
#end

unless Dir.exists?("#{Dir.home}/bin")
  puts "creating bin folder in home directory"
  FileUtils.mkdir "#{Dir.home}/bin"
end

unless File.exist?"#{Dir.home}/bin/composer"

  system %q(php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');")
  system %q(php -r "if (hash_file('SHA384', 'composer-setup.php') === '070854512ef404f16bac87071a6db9fd9721da1684cd4589b1196c3faf71b9a2682e2311b36a5079825e155ac7ce150d') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;")
  system %Q(php composer-setup.php --install-dir=#{Dir.home}/bin --filename=composer)
  system %q(php -r "unlink('composer-setup.php');")
end

composer_packages = [
  "laravel/installer"
]

composer_packages.each do |package|
  system "composer global require #{package}"
end
