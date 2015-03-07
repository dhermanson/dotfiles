" Setup Pathogen
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" Global settings
syntax on
filetype plugin indent on
set nocompatible
set tabstop=2
set shiftwidth=2
set noexpandtab
set number

" Color Scheme
colorscheme molokai
let g:rehash256=1

" Emmet Settings
" Only use for html, css
let g:user_emmet_install_global=0
autocmd FileType html,css EmmetInstall
