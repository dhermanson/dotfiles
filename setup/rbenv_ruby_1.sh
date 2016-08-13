git clone https://github.com/rbenv/rbenv.git ~/.rbenv

echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.zshrc

~/.rbenv/bin/rbenv init

git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build

echo "Now restart your shell and enter 'type rbenv' and 'rbenv install x.x.x'"
