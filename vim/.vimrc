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
set relativenumber
set noshowmode
set omnifunc=syntaxcomplete#Complete
set completefunc=syntaxcomplete#Complete
set mouse=a
set ttymouse=xterm2
"
" Sets the directory to store .swp files in.
" " The double '//' ensures that there will be no name conflicts 
" " amongst the swap files by replacing path separators with %
 set directory=~/.vim/tmp/swap//
" " You can also do this for the other directories
 set backupdir=~/.vim/tmp/backup//
 set undodir=~/.vim/tmp/undo//

" color scheme
set t_Co=256
let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : ''  }
set background=dark
"colorscheme jellybeans
colorscheme base16-chalk

"autocmd CompleteDone * pclose

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsListSnippets="<c-l>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" " If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

let mapleader=","
nmap <Leader>; :
imap jk <Esc>
nmap <Leader>w <C-w>

"buffer
map <Leader>a gg<S-v><S-g>

" syntax on
"map <Leader>.s :syntax on<CR>

"easymotion settings
let g:EasyMotion_smartcase = 1
nmap <Space> <Plug>(easymotion-s)
map <Leader><Space> <Plug>(easymotion-prefix)
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

" fugitive
map <Leader>gs :Gstatus<CR>

" nerdtree
map <Leader>n :NERDTreeToggle<CR>

" tagbar settings
map <Leader>b :TagbarToggle<CR>

" syntastic settings
set statusline+=%#warningsmsg#
set statusline+={SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=0
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0
let g:syntastic_ruby_checkers = ['mri']
let g:syntastic_javascript_checkers = ['eslint', 'jshint']
let g:syntastic_apiblueprint_checkers = ['drafter']

" table mode
let g:table_mode_corner = "|"

" dispatch
nmap <Leader>dp :Dispatch 
nmap <Leader>ds :Start 

" ctrlp settings
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = 'node_modules'
nnoremap <Leader>. :CtrlPTag<CR>

" delimitmate settings
let g:delimitMate_expand_cr=1
let g:delimitMate_expand_space=1

" airline settings
let g:airline_theme='base16'
let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
      let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1

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
" selects entire buffer
nmap <space>bs gg<S-v><S-g><C-c><C-C>

" typescript
autocmd FileType typescript setlocal completeopt-=menu,preview
"
" dbext configuration
let g:dbext_default_use_sep_result_buffer = 1
let g:dbext_default_window_use_horiz = 0  " Use vertical split
let g:dbext_default_window_use_right = 1   " Right
let g:dbext_default_window_width = 80

" rails
map <Leader>rr :Rake 
map <Leader>rev :Eview<CR>
map <Leader>rem :Emodel<CR>
map <Leader>rec :Econtroller<CR>
map <Leader>rsv :Sview<CR>
map <Leader>rsm :Smodel<CR>
map <Leader>rsc :Scontroller<CR>
map <Leader>rvv :Vview<CR>
map <Leader>rvm :Vmodel<CR>
map <Leader>rvc :Vcontroller<CR>
map <Leader>rgc :Rgenerate controller 

map <Leader>rev<Space> :Eview 
map <Leader>rem<Space> :Emodel 
map <Leader>rec<Space> :Econtroller 
map <Leader>rsv<Space> :Sview 
map <Leader>rsm<Space> :Smodel 
map <Leader>rsc<Space> :Scontroller 
map <Leader>rvv<Space> :Vview 
map <Leader>rvm<Space> :Vmodel 
map <Leader>rvc<Space> :Vcontroller 

" php
autocmd FileType php setlocal shiftwidth=2 tabstop=2 expandtab softtabstop=2

" vim-http-client
let g:http_client_bind_hotkey = 0
"let g:http_client_json_ft = 'json'
nmap <Leader>,r :HTTPClientDoRequest<CR>

