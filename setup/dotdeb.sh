#echo "deb http://packages.dotdeb.org jessie all" >> /etc/apt/sources.list
#echo "deb-src http://packages.dotdeb.org jessie all" >> /etc/apt/sources.list

wget https://www.dotdeb.org/dotdeb.gpg
sudo apt-key add dotdeb.gpg

sudo apt-get update
