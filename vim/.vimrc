set nocompatible
" setup pathogen
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" global settings
syntax on
filetype plugin indent on
set tabstop=2
set shiftwidth=2
set backspace=indent,eol,start
set expandtab
set number
set noshowmode
set omnifunc=syntaxcomplete#Complete
set completefunc=syntaxcomplete#Complete

autocmd CompleteDone * pclose

let mapleader="\<Space>"

"map <C-n> :NERDTreeToggle<CR>
map <Leader>tn :NERDTreeToggle<CR>

" tagbar settings
"nmap <F8> :TagbarToggle<CR>
map <Leader>tt :TagbarToggle<CR>

" color scheme
colorscheme jellybeans

" syntastic settings
set statusline+=%#warningsmsg#
set statusline+={SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=0
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0

" language specific checkers
let g:syntastic_ruby_checkers = ['mri']

" ctrlp settings
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = 'node_modules'


" delimitmate settings
let g:delimitMate_expand_cr=1
let g:delimitMate_expand_space=1

" airline settings
let g:airline_theme='jellybeans'

" vim-ruby settings
let g:rubycomplete_buffer_loading=1
let g:rubycomplete_classes_in_global=1
let g:rubycomplete_rails=1
let g:rubycomplete_load_gemfile=1

" surround settings for erb templates
autocmd FileType erb let b:surround_{char2nr('=')} = "<%= \r %>"
autocmd FileType erb let b:surround_{char2nr('-')} = "<% \r %>"

" indent-guides settings
let g:indent_guides_enable_on_vim_startup = 0
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 2

" tern_for_vim settings
set updatetime=1000
let g:tern_show_argument_hints = 'on_hold'
let g:tern_show_signature_in_pum = 1

" sql_complete settings
let g:ftplugin_sql_omni_key = '<C-C>'
let g:sql_type_default = 'pgsql'

" vim-slime
let g:slime_target = "tmux"
nmap <space>msb gg<S-v><S-g><C-c><C-C>

" typescript
autocmd FileType typescript setlocal completeopt-=menu,preview
