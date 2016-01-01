#!/bin/sh

# If you would like to do some extra provisioning you may
# add any commands you wish to this file and they will
# be run after the Homestead machine is provisioned.

# Install Xdebug
git clone git://github.com/xdebug/xdebug.git
cd xdebug
phpize
./configure --enable-xdebug
make
make install

# Configure Xdebug
cat > /etc/php/mods-available/xdebug.ini <<EOL
zend_extension=xdebug.so
xdebug.default_enable=1
xdebug.remote_enable=1
xdebug.remote_port=9000
xdebug.remote_host=10.10.0.58
xdebug.remote_autostart=1
xdebug.remote_connect_back=1
EOL
ln -s /etc/php/mods-available/xdebug.ini /etc/php/7.0/fpm/conf.d/20-xdebug.ini
sudo service php7.0-fpm restart

cat >> /etc/hhvm/php.ini <<EOL

xdebug.enable=1
xdebug.default_enable=1
xdebug.remote_enable=1
xdebug.remote_connect_back=1
xdebug.remote_autostart=1
xdebug.idekey="PHPSTORM"
xdebug.remote_host=10.10.0.58
xdebug.remote_port=9000
EOL
sudo service hhvm restart

# Setup mailcatcher
sudo apt-get install ruby --assume-yes
sudo apt-get install ruby-dev --assume-yes
sudo apt-get install libsqlite3-dev --assume-yes
sudo gem install mailcatcher
mailcatcher --smtp-ip=127.0.0.1 --http-ip=192.168.10.10

# Setup PsySH
su vagrant -c "composer g require psy/psysh:@stable"
su vagrant -c "mkdir -p /home/vagrant/.local/share/psysh"
su vagrant -c "wget --directory-prefix=/home/vagrant/.local/share/psysh http://psysh.org/manual/en/php_manual.sqlite"

