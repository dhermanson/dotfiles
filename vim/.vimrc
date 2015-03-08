" setup pathogen
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" global settings
syntax on
filetype plugin indent on
set nocompatible
set tabstop=2
set shiftwidth=2
set noexpandtab
set number

" color scheme
colorscheme molokai
let g:rehash256=1
"colorscheme jellybeans

" emmet settings
" Only use for html, css
let g:user_emmet_install_global=0
autocmd FileType html,css EmmetInstall

" syntastic settings
set statusline+=%#warningsmsg#
set statusline+={SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0

" vim-tmux-navigator settings
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <c-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <c-j> :TmuxNavigateDown<cr>
nnoremap <silent> <c-k> :TmuxNavigateUp<cr>
nnoremap <silent> <c-l> :TmuxNavigateRight<cr>
"nnoremap <silent> {Previous-Mapping} :TmuxNavigatePrevious<cr>

" ctrlp settings
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'

" tagbar settings
nmap <F8> :TagbarToggle<CR>

" delimitmate settings
let g:delimitMate_expand_cr=1
let g:delimitMate_expand_space=1

" airline settings
let g:airline_theme='jellybeans'

" NERDtree settings
map <C-n> :NERDTreeToggle<CR>
