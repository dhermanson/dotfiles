function create_php_ctags() {
  ctags -R --PHP-kinds=cf --languages=php --exclude=vendor
}

function create_php_vendor_ctags() {
  ctags -R --PHP-kinds=cf --languages=php -f vendortags vendor
}

function create_ruby_ctags() {
  ctags -R --languages=ruby
}
