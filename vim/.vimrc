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

" use html autocompletion in html and eruby files
autocmd FileType html,eruby set omnifunc=htmlcomplete#CompleteTags

" color scheme
colorscheme molokai
let g:rehash256=1
"colorscheme jellybeans

" supertab settings
let g:SuperTabDefaultCompletionType="<C-X><C-O>"

" syntastic settings
set statusline+=%#warningsmsg#
set statusline+={SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=0
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

" vim-ruby settings
let g:rubycomplete_buffer_loading=1
let g:rubycomplete_classes_in_global=1
let g:rubycomplete_rails=1
let g:rubycomplete_load_gemfile=1
