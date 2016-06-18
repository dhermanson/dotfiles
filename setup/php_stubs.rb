require 'fileutils'

unless Dir.exists?("#{Dir.home}/.repositories/github/JetBrains/phpstorm-stubs")
  puts "cloning my phpstorm-stubs repo"
  system "git clone https://github.com/JetBrains/phpstorm-stubs #{Dir.home}/.repositories/github/JetBrains/phpstorm-stubs"
end

unless Dir.exists?("#{Dir.home}/tags")
  FileUtils.mkdir "#{Dir.home}/tags"
  fork do
    Dir.chdir("#{Dir.home}/tags")
    system "ctags -R --fields=K --PHP-kinds=ztxy --languages=php -f tags.php #{Dir.home}/.repositories/github/JetBrains/phpstorm-stubs"
  end
end
