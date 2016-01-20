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

" Sets the directory to store .swp files in.
" The double '//' ensures that there will be no name conflicts
" amongst the swap files by replacing path separators with %
set directory=~/.vim/tmp/swap//
" You can also do this for the other directories
set backupdir=~/.vim/tmp/backup//
set undodir=~/.vim/tmp/undo//

" color scheme
set t_Co=256
let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : ''  }
set background=dark
"colorscheme jellybeans
colorscheme base16-chalk


"autocmd CompleteDone * pclose



let mapleader=" "
let maplocalleader = ","
nnoremap <Leader>; :
inoremap jk <Esc>
nnoremap <Leader>w <C-w>

"buffer
nnoremap <Leader>ba gg<S-v><S-g>
nnoremap <Leader>bs :w<CR>
nnoremap <Leader>bd :bdelete<CR>
nnoremap <Leader>bn :new<CR>
nnoremap <Leader>bvn :vnew<CR>

"tabs
"noremap <Leader><s-tab> :tabprevious<CR>
"noremap <Leader><tab> :tabnext<CR>

" syntax on
"noremap <Leader>.s :syntax on<CR>

" ack.vim
let g:ack_use_dispatch = 1

"easymotion settings
let g:EasyMotion_smartcase = 1
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)
nmap <Leader>f <Plug>(easymotion-s)
"nmap f <Plug>(easymotion-sl)
nmap <Leader><Leader>w <Plug>(easymotion-bd-w)
nmap <Leader><Leader>e <Plug>(easymotion-bd-e)

" fugitive
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>ge :Gedit<CR>
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gw :Gwrite<CR>

" netrw
nnoremap <Leader>e.<space> :edit .<CR>
nnoremap <Leader>e.s :split . <CR>
nnoremap <Leader>e.v :vsplit . <CR>

nnoremap <Leader>e<space> :Explore<CR>
nnoremap <Leader>es :SExplore<CR>
nnoremap <Leader>ev :VExplore<CR>

" tagbar settings
nnoremap <Leader>0 :TagbarToggle<CR>

" syntastic settings
set statusline+=%#warningsmsg#
set statusline+={SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_error_symbol = "✗"
let g:syntastic_warning_symbol = "⚠"
let g:syntastic_debug = 0
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=0
let g:syntastic_check_on_open=0
let g:syntastic_check_on_wq=0
let g:syntastic_aggregate_errors=1
let g:syntastic_ruby_checkers = ['mri']
let g:syntastic_javascript_checkers = ['eslint']
"let g:syntastic_apiblueprint_checkers = ['drafter']
"let g:syntastic_apiblueprint_drafter_exec = "/usr/local/bin/drafter"
"let g:syntastic_python_python_exec = '/usr/local/bin/python3'
let g:syntastic_python_python_exec = 'python3'
let g:syntastic_python_checkers = ['mypy', 'python']
"let g:syntastic_python_mypy_exec = '/usr/local/bin/mypy'
"let g:syntastic_typescript_tsc_args = '--module commonjs --target ES5 --experimentalDecorators'
let g:syntastic_typescript_tsc_fname = ''
"let g:syntastic_elixir_checkers = ['elixir']

" table mode
let g:table_mode_corner = "|"

" dispatch
nnoremap <Leader>dp :Dispatch 
nnoremap <Leader>ds :Start 

" ctrlp settings
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
"let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
"let g:ctrlp_custom_ignore = 'node_modules'
let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist)|(\.(swp|ico|git|svn))$'
let g:ctrlp_show_hidden = 1
nnoremap <Leader>pf :CtrlP<CR>
nnoremap <Leader>pt :CtrlPTag<CR>
nnoremap <Leader>pb :CtrlPBuffer<CR>

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
augroup filetype_erb
  autocmd!
  autocmd FileType erb let b:surround_{char2nr('=')} = "<%= \r %>"
  autocmd FileType erb let b:surround_{char2nr('-')} = "<% \r %>"
augroup END

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
" selects entire buffer and slimes it
"nnoremap <space>bs gg<S-v><S-g><C-c><C-C>

" typescript
"autocmd FileType typescript setlocal completeopt-=menu,preview

" dbext configuration
let g:dbext_default_use_sep_result_buffer = 1
let g:dbext_default_window_use_horiz = 0  " Use vertical split
let g:dbext_default_window_use_right = 1   " Right
let g:dbext_default_window_width = 80

" TODO: clean this rails stuff up
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
augroup derick_php
  autocmd!
  autocmd FileType php setlocal shiftwidth=2 tabstop=2 expandtab softtabstop=2
augroup END
let g:phpcomplete_parse_docblock_comments = 1

" python
let g:jedi#auto_close_doc = 0

" vim-rest-console
let g:vrc_trigger = '<Leader>mr'


nnoremap <Leader>.p :set paste!<CR>
nnoremap <Leader>.ev :vsplit $MYVIMRC<CR>
nnoremap <Leader>.sv :source $MYVIMRC<CR>

vnoremap <Leader>,b64e :!python -m base64 -e<CR>
vnoremap <Leader>,b64d :!python -m base64 -d<CR>

nnoremap - ddp
nnoremap _ ddk<s-p>


augroup html
  autocmd!
  autocmd BufWritePre,BufRead *.html :normal; gg=G
  autocmd BufNewFile,BufRead *.html setlocal nowrap
  autocmd FileType html nnoremap <buffer> <localleader>e :echo "You've opened a html file!"<CR>
augroup END
augroup javascript
  autocmd FileType javascript nnoremap <buffer> <localleader>e :echo "You've opened a javascript file!"<CR>
  autocmd Filetype *.txt set spell
augroup END


function! SetupDebugger()
  let g:vdebug_options['path_maps'] = {"/home/vagrant/Code": "/Users/derick/workspace/homestead/code"}
endfunction

"let g:pymode_rope_autoimport = 1
"let g:pymode_run_bind = '<localleader>r'
"let g:pymode_breakpoint_bind = '<localleader>b'
"let g:pymode_lint = 0
"let g:pymode_lint_cwindow = 0
"let g:pymode_lint_checkers = []

" view
set viewdir=$HOME/.vim_view//
au BufWritePost,BufLeave,WinLeave *.rest mkview " for tabs
au BufWinEnter *.rest silent loadview


augroup typescript
  autocmd!
  " tsuquyomi
  autocmd FileType typescript nnoremap <localleader>d :TsuquyomiDefinition<CR>
  autocmd FileType typescript nnoremap <localleader>r :TsuquyomiReferences<CR>
  autocmd FileType typescript nnoremap <localleader>c :TsuquyomiRenameSymbolC<CR>
  autocmd FileType typescript nnoremap <localleader>b :TsuquyomiGoBack<CR>
  autocmd FileType typescript nnoremap <localleader>ef :TsuquyomiGeterr<CR>
  autocmd FileType typescript nnoremap <localleader>ep :TsuquyomiGeterrProject<CR>
  autocmd FileType typescript nnoremap <localleader>t :YcmCompleter GetType<CR>
  autocmd FileType typescript setlocal completeopt+=menu,preview
  "autocmd FileType typescript nnoremap <buffer> <localleader>r :call RunTypescriptFile()<CR>
augroup END

" ctags
let g:tagbar_type_typescript = {
  \ 'ctagstype': 'typescript',
  \ 'kinds': [
    \ 'c:classes',
    \ 'n:modules',
    \ 'f:functions',
    \ 'v:variables',
    \ 'v:varlambdas',
    \ 'm:members',
    \ 'i:interfaces',
    \ 'e:enums',
  \ ]
\ }

let g:tagbar_type_snippets = {
    \ 'ctagstype' : 'snippets',
    \ 'kinds' : [
        \ 's:snippets',
    \ ]
\ }

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsListSnippets="<c-l>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:UltiSnipsEditSplit="vertical"
nnoremap <Leader>.es :UltiSnipsEdit<CR>
nnoremap <Leader>.eas :e ~/.vim/Ultisnips/all.snippets<CR>

nnoremap <Leader>.os :syntax on<CR>

nnoremap <Leader>.sc :SyntasticCheck<CR>
nnoremap <Leader>.st :SyntasticToggleMode<CR>
