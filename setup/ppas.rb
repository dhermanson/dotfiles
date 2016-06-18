ppas = [
  "ppa:neovim-ppa/unstable"
]

ppas.each do |ppa|
  system "sudo add-apt-repository #{ppa}"
end

system "sudo apt-get update"
