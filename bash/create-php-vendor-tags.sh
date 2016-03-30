#! /bin/bash

ctags -R --PHP-kinds=cfti --fields=+l --languages=php -f vendortags vendor
