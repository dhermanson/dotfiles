sudo apt-get install libncurses5-dev libgnome2-dev libgnomeui-dev \
    libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
    libcairo2-dev libx11-dev libxpm-dev libxt-dev python-dev \
    git

git clone https://github.com/vim/vim ~/.repos/github/vim
pushd .

cd ~/.repos/github/vim

./configure --prefix=/usr/local \
    --enable-gui=gtk2 \
    --enable-cscope \
    --disable-nls \
    --enable-multibyte \
    --with-tlib=ncurses \
    --enable-pythoninterp \
    --enable-rubyinterp \
    --with-ruby-command=/usr/bin/ruby \
    --with-features=huge

make
sudo make install

popd
