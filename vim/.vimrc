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
set complete=.,w,b,u
set autowriteall
set cursorline
set nocursorcolumn
set noswapfile

"-----------split management----------------------- 
set nosplitbelow
set splitright

" move between splits by holding ctrl
nnoremap <silent> <c-h> <c-w>h
nnoremap <silent> <c-j> <c-w>j
nnoremap <silent> <c-k> <c-w>k
nnoremap <silent> <c-l> <c-w>l

" apply macros with Q
nnoremap Q @q
vnoremap Q :norm @q<cr>

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
colorscheme jellybeans
"

" fastfold
"let g:tex_fold_enabled=1
"let g:vimsyn_folding='af'
"let g:xml_syntax_folding = 1
"let g:php_folding = 1
"let g:perl_fold = 1

let mapleader=" "
let maplocalleader = ","
nnoremap ; :
nnoremap ' ;
vnoremap ' ;
inoremap jk <Esc>
"nnoremap <Leader>w <C-w>

"buffer
nnoremap <return> :w<CR>
nnoremap <Leader>q :bdelete<CR>
nnoremap <Leader>x :call ConfirmBDeleteBang()<CR>
nnoremap <Leader>ns :new<CR>
nnoremap <Leader>nv :vnew<CR>

"window stuff
nnoremap <Leader>wh <C-w>H
nnoremap <Leader>wj <C-w>J
nnoremap <Leader>wk <C-w>K
nnoremap <Leader>wl <C-w>L
nnoremap <Leader>wo <C-w>o
nnoremap <Leader>wc <C-w>c
nnoremap <Leader>ww <C-w>w
nnoremap <Leader>ws :split <CR>
nnoremap <Leader>wv :vsplit <CR>
nnoremap <Leader>wa gg<S-v><S-g> " highlight all


"tabs
"noremap <Leader><s-tab> :tabprevious<CR>
"noremap <Leader><tab> :tabnext<CR>

" syntax on
"noremap <Leader>.s :syntax on<CR>

nnoremap <Leader>oc :copen <CR>

" ack.vim
let g:ack_use_dispatch = 1
nnoremap <Leader>8 :Ack 
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

"easymotion settings
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_smartcase = 1
map <silent> / <Plug>(easymotion-sn)
omap <silent> / <Plug>(easymotion-tn)
"map <Leader>' <Plug>(easymotion-bd-f)
map <Leader>; <Plug>(easymotion-bd-f)
"nmap <silent> f <Plug>(easymotion-overwin-bd-f)
"nmap <silent> s <Plug>(easymotion-overwin-f2)
"nmap f <Plug>(easymotion-sl)
"nmap <Leader><Leader>w <Plug>(easymotion-bd-w)
"nmap <Leader><Leader>e <Plug>(easymotion-bd-e)

" fugitive
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>ge :Gedit<CR>
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gw :Gwrite<CR>
set statusline+=%{fugitive#statusline()}

" netrw
nnoremap <Leader>1 :edit .<CR>
nnoremap <Leader>2 :Explore<CR>
nnoremap <Leader>3 :Sexplore<CR>
nnoremap <Leader>4 :Vexplore<CR>

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
let g:syntastic_typescript_checkers = ['']
let g:syntastic_php_checkers = ['php', 'phpmd'] " php, phpcs, phpmd, phplint
let g:syntastic_phpcs_conf = '--standard=psr2 --config-set show_warnings 0'

"let g:syntastic_elixir_checkers = ['elixir']

" table mode
let g:table_mode_corner = "|"
let g:table_mode_map_prefix = '<Leader>.t'

" dispatch
nnoremap <Leader>dp :Dispatch <CR>
nnoremap <Leader>ds :Start <CR>
vnoremap <Leader>dp y:call DispatchCommand(@@)<CR>
vnoremap <Leader>ds y:call DispatchCommand(@@, "Start")<CR>

" tags
"nnoremap <Leader>lt :tag<space>
nnoremap <Leader>t :tselect 
set tags+=tags.vendor

" ctrlp settings
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir', 'rtscript',
                          \ 'undo', 'line', 'changes', 'mixed', 'bookmarkdir']
"let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
"let g:ctrlp_custom_ignore = 'node_modules'
let g:ctrlp_custom_ignore = '\v[\/](vendor|node_modules|target|dist)|(\.(swp|ico|git|svn))$'
let g:ctrlp_show_hidden = 1
let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:50,results:30'
let g:ctrlp_buftag_types = {
    \ 'php'        : '--fields=K --PHP-kinds=mctdfip --languages=php',
  \ }

nnoremap <Leader>f :CtrlP<CR>
"nnoremap <Leader>lmru :CtrlPMRUFiles<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>a :CtrlPTag<CR>
nnoremap <Leader>l :CtrlPBufTag<CR>
nnoremap <Leader>k :CtrlPBufTagAll<CR>
"nnoremap <Leader>ld :CtrlPDir<CR>

" delimitmate settings
let g:delimitMate_expand_cr=1
let g:delimitMate_expand_space=1

" airline settings
let g:airline_theme='jellybeans'
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

" dbext configuration
" TODO: configure dbext, cuz i just turned off all mapping with
"       the line below this one
let g:dbext_default_usermaps = 1
let g:dbext_default_use_sep_result_buffer = 1
let g:dbext_default_buffer_lines = 25
"let g:dbext_default_window_use_horiz = 0  " Use vertical split
"let g:dbext_default_window_use_right = 1   " Right
"let g:dbext_default_window_width = 80

" python
let g:jedi#auto_close_doc = 0

" vim-rest-console
let g:vrc_trigger = '<Leader>.mr'


nnoremap <Leader>.p :set paste!<CR>
nnoremap <Leader>.ev :e $MYVIMRC<CR>
nnoremap <Leader>.sv :source $MYVIMRC<CR>

"vnoremap <Leader>,b64e :!python -m base64 -e<CR>
"vnoremap <Leader>,b64d :!python -m base64 -d<CR>

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



" tagbar settings
nnoremap <Leader>0 :TagbarToggle<CR>
nnoremap <Leader>9 :TagbarTogglePause<CR>
let g:tagbar_type_ruby = {
    \ 'kinds' : [
        \ 'm:modules',
        \ 'c:classes',
        \ 'd:describes',
        \ 'C:contexts',
        \ 'f:methods',
        \ 'F:singleton methods'
    \ ]
\ }

let g:tagbar_type_elixir = {
    \ 'ctagstype' : 'elixir',
    \ 'kinds' : [
        \ 'f:functions',
        \ 'functions:functions',
        \ 'c:callbacks',
        \ 'd:delegates',
        \ 'e:exceptions',
        \ 'i:implementations',
        \ 'a:macros',
        \ 'o:operators',
        \ 'm:modules',
        \ 'p:protocols',
        \ 'r:records'
    \ ]
\ }

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

let g:tagbar_type_php  = {
    \ 'ctagstype' : 'php',
    \ 'kinds'     : [
        \ 'i:interfaces',
        \ 'c:classes',
        \ 'd:constants',
        \ 'm:methods',
        \ 'f:functions',
        \ 't:traits',
        \ 'p:properties',
        \ 'r:static_properties',
        \ 'x:static_methods',
        \ 'z:p_functions'
    \ ]
  \ }

let g:tagbar_type_markdown = {
    \ 'ctagstype' : 'markdown',
    \ 'kinds' : [
        \ 'h:Heading_L1',
        \ 'i:Heading_L2',
        \ 'k:Heading_L3'
    \ ]
\ }


" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsListSnippets="<c-l>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:UltiSnipsEditSplit="vertical"
nnoremap <Leader>.es :UltiSnipsEdit<CR>
nnoremap <Leader>.eas :e ~/.vim/Ultisnips/all.snippets<CR>

nnoremap <Leader>.os :syntax on<CR>

nnoremap <Leader>.sc :SyntasticCheck<CR>
nnoremap <Leader>.st :SyntasticToggleMode<CR>




vnoremap <Leader>be y:call Base64Encode(@@)<CR>
vnoremap <Leader>bd y:call Base64Decode(@@)<CR>

nnoremap <Leader>,jt :%!python -m json.tool<CR>


nnoremap <Leader>,a yiw:call AckSearchWord(@@, '.')<CR>





" php documentor snippets location
let g:pdv_template_dir = $HOME ."/.vim/bundle/pdv/templates_snip"

"------------php cs-fixer----------------------------------------
let g:php_cs_fixer_level = "symfony"              " which level ?
let g:php_cs_fixer_config = "default"             " configuration
"let g:php_cs_fixer_config_file = '.php_cs'       " configuration file
let g:php_cs_fixer_php_path = "php"               " Path to PHP
" If you want to define specific fixers:
let g:php_cs_fixer_fixers_list = "-braces,-psr0"  " need psr0 here because laravel's app/ should be App
let g:php_cs_fixer_enable_default_mapping = 0     " Enable the mapping by default (<leader>pcd)
let g:php_cs_fixer_dry_run = 0                    " Call command with dry-run option
let g:php_cs_fixer_verbose = 1                    " Return the output of command if 1, else an inline information.
"----------------------------------------------------------------

" markown
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_json_frontmatter = 1
let g:vim_markdown_conceal = 0

"-------- Functions ------------------------------- 
function! ConfirmBDeleteBang()
  let l:choice = confirm("Really delete buffer?", "&Yes\n&No")
  if l:choice == 1
    execute "bdelete!"
  endif
endfunction

function! AckSearchWord(word, directory)
  execute "Ack " . a:word . " " . a:directory
endfunction

function! Base64Encode(value)
ruby <<EOF
  require 'base64'

  value = Vim::evaluate('a:value').strip
  encoded = Base64.encode64(value).strip
  line = Vim::Buffer.current.line_number

  Vim::Buffer.current.append(line, encoded)
EOF
endfunction

function! Base64Decode(value)
ruby <<EOF
  require 'base64'

  value = Vim::evaluate('a:value').strip
  encoded = Base64.decode64(value).strip
  line = Vim::Buffer.current.line_number

  Vim::Buffer.current.append(line, encoded)
EOF
endfunction

function! DispatchCommand(command, ...)
  let l:choice = confirm("Execute command: " . a:command, "&Yes\n&No")
  let l:dispatch = a:0 ? a:1 : "Dispatch"

  if l:choice == 1
    execute l:dispatch " " . a:command
  endif
endfunction

function! WriteNumberList(numbers)
  ruby <<EOL
  buffer = Vim::Buffer.current
  current_line = buffer.line_number
  numbers = Vim::evaluate("a:numbers")

  numbers.times do |num|
    Vim::Buffer.current.append current_line, num.to_s
    current_line += 1
  end
EOL
endfunction

function! IPhpInsertUse()
    call PhpInsertUse()
    call feedkeys('a',  'n')
endfunction

function! IPhpExpandClass()
    call PhpExpandClass()
    call feedkeys('a', 'n')
endfunction

function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction
"-------------Auto-Commands--------------"

augroup filetype_css
  autocmd!
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
augroup END

" surround settings for erb templates
augroup filetype_erb
  autocmd!
  autocmd FileType erb let b:surround_{char2nr('=')} = "<%= \r %>"
  autocmd FileType erb let b:surround_{char2nr('-')} = "<% \r %>"
augroup END

" php
augroup my_php
  autocmd!
  autocmd FileType php setlocal shiftwidth=2 tabstop=2 expandtab softtabstop=2
  "autocmd FileType php setlocal tags+=~/tags/tags.php
  autocmd FileType php nnoremap <localleader>mtp :Dispatch create-php-ctags.sh<CR>
  autocmd FileType php nnoremap <localleader>mtv :Dispatch create-php-vendor-tags.sh<CR>
augroup END

augroup phpNamespaces
  autocmd!
  autocmd FileType php inoremap <localleader>n <Esc>:call IPhpInsertUse()<CR>
  autocmd FileType php noremap <localleader>n :call PhpInsertUse()<CR>
  autocmd FileType php inoremap <localleader>q <Esc>:call IPhpExpandClass()<CR>
  autocmd FileType php noremap <localleader>q :call PhpExpandClass()<CR>
  autocmd FileType php inoremap <localleader>.s <Esc>:call PhpSortUse()<CR>
  autocmd FileType php noremap <localleader>.s :call PhpSortUse()<CR>

  autocmd FileType php nnoremap <localleader>emo :Emodel 
  autocmd FileType php nnoremap <localleader>vmo :Vmodel 
  autocmd FileType php nnoremap <localleader>smo :Smodel 
  autocmd FileType php nnoremap <localleader>eev :Eevent 
  autocmd FileType php nnoremap <localleader>vev :Vevent 
  autocmd FileType php nnoremap <localleader>sev :Sevent 
  autocmd FileType php nnoremap <localleader>eex :Eexception 
  autocmd FileType php nnoremap <localleader>vex :Vexception 
  autocmd FileType php nnoremap <localleader>sex :Sexception 
  autocmd FileType php nnoremap <localleader>ero :Eroutes <CR>
  autocmd FileType php nnoremap <localleader>vro :Vroutes <CR>
  autocmd FileType php nnoremap <localleader>sro :Sroutes <CR>
  autocmd FileType php nnoremap <localleader>ek :Ekernel <CR>
  autocmd FileType php nnoremap <localleader>vk :Vkernel <CR>
  autocmd FileType php nnoremap <localleader>sk :Skernel <CR>
  autocmd FileType php nnoremap <localleader>eco :Econtroller 
  autocmd FileType php nnoremap <localleader>vco :Vcontroller 
  autocmd FileType php nnoremap <localleader>sco :Scontroller 
  autocmd FileType php nnoremap <localleader>emid :Emiddleware 
  autocmd FileType php nnoremap <localleader>vmid :Vmiddleware 
  autocmd FileType php nnoremap <localleader>smid :Smiddleware 
  autocmd FileType php nnoremap <localleader>ere :Erequest 
  autocmd FileType php nnoremap <localleader>vre :Vrequest 
  autocmd FileType php nnoremap <localleader>sre :Srequest 
  autocmd FileType php nnoremap <localleader>ej :Ejob 
  autocmd FileType php nnoremap <localleader>vj :Vjob 
  autocmd FileType php nnoremap <localleader>sj :Sjob 
  autocmd FileType php nnoremap <localleader>el :Elistener 
  autocmd FileType php nnoremap <localleader>vl :Vlistener 
  autocmd FileType php nnoremap <localleader>sl :Slistener 
  autocmd FileType php nnoremap <localleader>epo :Epolicy 
  autocmd FileType php nnoremap <localleader>vpo :Vpolicy 
  autocmd FileType php nnoremap <localleader>spo :Spolicy 
  autocmd FileType php nnoremap <localleader>epr :Eprovider 
  autocmd FileType php nnoremap <localleader>vpr :Vprovider 
  autocmd FileType php nnoremap <localleader>spr :Sprovider 
  autocmd FileType php nnoremap <localleader>ecfg :Econfig 
  autocmd FileType php nnoremap <localleader>vcfg :Vconfig 
  autocmd FileType php nnoremap <localleader>scfg :Sconfig 
  autocmd FileType php nnoremap <localleader>emig :Emigration 
  autocmd FileType php nnoremap <localleader>vmig :Vmigration 
  autocmd FileType php nnoremap <localleader>smig :Smigration 
  autocmd FileType php nnoremap <localleader>ese :Eseeder 
  autocmd FileType php nnoremap <localleader>vse :Vseeder 
  autocmd FileType php nnoremap <localleader>sse :Sseeder 
  autocmd FileType php nnoremap <localleader>ed :Edoc 
  autocmd FileType php nnoremap <localleader>vd :Vdoc 
  autocmd FileType php nnoremap <localleader>sd :Sdoc 
  autocmd FileType php nnoremap <localleader>ev :Eview 
  autocmd FileType php nnoremap <localleader>vv :Vview 
  autocmd FileType php nnoremap <localleader>sv :Sview 
  autocmd FileType php nnoremap <localleader>etr :Etransformer 
  autocmd FileType php nnoremap <localleader>vtr :Vtransformer 
  autocmd FileType php nnoremap <localleader>str :Stransformer 
  autocmd FileType php nnoremap <localleader>ete :Etest 
  autocmd FileType php nnoremap <localleader>vte :Vtest 
  autocmd FileType php nnoremap <localleader>ste :Stest 
  autocmd FileType php nnoremap <localleader>ea :A<CR>
  autocmd FileType php nnoremap <localleader>sa :AS<CR>
  autocmd FileType php nnoremap <localleader>va :AV<CR>
augroup END

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END

augroup phpDocumentor
  autocmd!
  autocmd FileType php nnoremap <localleader>d :call pdv#DocumentWithSnip()<CR>
augroup END

augroup phpCsFixer
  autocmd!
  autocmd FileType php nnoremap <localleader>cs :call PhpCsFixerFixFile()<CR>
augroup END

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

augroup my_ruby
  autocmd!
  " rails
  autocmd FileType ruby nnoremap <localleader>rr :Rake 
  autocmd FileType ruby nnoremap <localleader>rev :Eview<CR>
  autocmd FileType ruby nnoremap <localleader>rem :Emodel<CR>
  autocmd FileType ruby nnoremap <localleader>rec :Econtroller<CR>
  autocmd FileType ruby nnoremap <localleader>rsv :Sview<CR>
  autocmd FileType ruby nnoremap <localleader>rsm :Smodel<CR>
  autocmd FileType ruby nnoremap <localleader>rsc :Scontroller<CR>
  autocmd FileType ruby nnoremap <localleader>rvv :Vview<CR>
  autocmd FileType ruby nnoremap <localleader>rvm :Vmodel<CR>
  autocmd FileType ruby nnoremap <localleader>rvc :Vcontroller<CR>
  autocmd FileType ruby nnoremap <localleader>rgc :Rgenerate controller 

  autocmd FileType ruby nnoremap <localleader>rev<Space> :Eview 
  autocmd FileType ruby nnoremap <localleader>rem<Space> :Emodel 
  autocmd FileType ruby nnoremap <localleader>rec<Space> :Econtroller 
  autocmd FileType ruby nnoremap <localleader>rsv<Space> :Sview 
  autocmd FileType ruby nnoremap <localleader>rsm<Space> :Smodel 
  autocmd FileType ruby nnoremap <localleader>rsc<Space> :Scontroller 
  autocmd FileType ruby nnoremap <localleader>rvv<Space> :Vview 
  autocmd FileType ruby nnoremap <localleader>rvm<Space> :Vmodel 
  autocmd FileType ruby nnoremap <localleader>rvc<Space> :Vcontroller 

  autocmd FileType ruby nnoremap <localleader>mtp :Dispatch create-ruby-ctags.sh<CR>
augroup END

augroup my_elixir
  autocmd!
  autocmd FileType elixir setlocal tags+=~/tags/tags.elixir
augroup END

inoremap <c-space> <esc>:CtrlPBufTagAll <CR>
