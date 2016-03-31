#! /bin/bash

ctags -R --fields=K --PHP-kinds=mctdfip --languages=php -f vendortags vendor
