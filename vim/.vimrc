" Setup Pathogen
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" Setup syntax and filetype
syntax on
filetype plugin indent on

" Global settings for indentation
set tabstop=2
set shiftwidth=2
set noexpandtab

" Show line numbers
set number

" Emmet Settings
" Only use for html, css
let g:user_emmet_install_global=0
autocmd FileType html,css EmmetInstall
