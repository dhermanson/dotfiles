unless Dir.exists?("#{Dir.home}/.repositories/github/powerline/fonts")
  puts "cloning powerline repo"
  system "git clone --recursive https://github.com/powerline/fonts #{Dir.home}/.repositories/github/powerline/fonts"

  fork do
    Dir.chdir("#{Dir.home}/.repositories/github/powerline/fonts")
    system "./install.sh"
  end

  Process.wait
end
