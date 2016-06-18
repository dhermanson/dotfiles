require 'pathname'
require 'fileutils'

debs = {
  erlang: {
    url: "https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb",
  }
}

debs.values.each do |pkg|
  # TODO: don't download this every time script is run
  # you should check the repositories list first to see
  # if repo already added to system
  deb = Pathname.new(pkg[:url]).basename
  system "wget #{pkg[:url]}"
  system "sudo dpkg -i #{deb}"
  FileUtils.rm deb
end

# refresh repository cache
system "sudo apt-get update"
