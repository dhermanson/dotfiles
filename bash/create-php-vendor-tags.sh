#! /bin/bash

ctags -R --fields=K --PHP-kinds=cti --languages=php --exclude=test --exclude=tests --exclude=spec --exclude=specs -f tags.vendor vendor
