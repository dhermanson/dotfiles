set nocompatible

" setup pathogen
filetype off
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" add fzf to runtime path
set rtp+=~/.fzf

" global settings
syntax on
filetype plugin indent on

if has('gui_macvim') && has('gui_running')
  set macmeta
endif

set timeout timeoutlen=1000 ttimeoutlen=100
nnoremap <BS> :e 

set tabstop=2
set shiftwidth=2
set backspace=indent,eol,start
set expandtab
set number
set relativenumber
set noshowmode
"set omnifunc=syntaxcomplete#Complete
"set completefunc=syntaxcomplete#Complete
set mouse=a
set complete=.,w,b,u
set autowriteall
set nocursorline
set nocursorcolumn
set noswapfile
set nohlsearch

" no bells
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

let base16colorspace=256  " Access colors present in 256 colorspace"
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors"
set background=dark
"colorscheme base16-tomorrow
let g:gruvbox_italic=0
let g:gruvbox_invert_signs=1
"let g:gruvbox_contrast_dark='soft'
"let g:gruvbox_contrast_light='soft'
colorscheme gruvbox
"highlight Comment cterm=italic

vnoremap <C-g> <esc>
cnoremap <C-g> <C-c>

nnoremap <silent> <C-e> $
nnoremap <silent> <C-a> ^

inoremap <silent> <C-e> <esc>$a
inoremap <silent> <C-a> <esc>^i


"-------------------------------------------------------------------------------
" stole this stuff from nick nisi's dotfiles...see what these do
"-------------------------------------------------------------------------------
"faster redrawing
set ttyfast
set magic " Set magic on, for regex

set showmatch " show matching braces
set mat=2 " how many tenths of a second to blink
set encoding=utf8
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"map <silent> <C-h> :call WinMove('h')<cr>
"if has('nvim')
  "map <silent> <bs> :call WinMove('h')<cr>
"endif
"map <silent> <C-j> :call WinMove('j')<cr>
"map <silent> <C-k> :call WinMove('k')<cr>
"map <silent> <C-l> :call WinMove('l')<cr>
map <silent> <M-h> :call WinMove('h')<cr>
map <silent> <M-j> :call WinMove('j')<cr>
map <silent> <M-k> :call WinMove('k')<cr>
map <silent> <M-l> :call WinMove('l')<cr>

" Window movement shortcuts
" move to the window in the direction shown, or create a new window
function! WinMove(key)
    let t:curwin = winnr()
    exec "wincmd ".a:key
    if (t:curwin == winnr())
        if (match(a:key,'[jk]'))
            wincmd v
        else
            wincmd s
        endif
        exec "wincmd ".a:key
    endif
endfunction

"-------------------------------------------------------------------------------

"------------------------------------
nnoremap <M-o> <C-w>o

if !has('nvim')
  set ttymouse=xterm2
endif

if !has('gui_running')
  if !has('nvim')
    "map vim escape sequences as explained in
    "http://stackoverflow.com/questions/6778961/alt-key-shortcuts-not-working-on-gnome-terminal-with-vim
    let c='a'
    while c <= 'z'
      exec "set <A-".c.">=\e".c
      "exec "imap \e".c." <A-".c.">"
      let c = nr2char(1+char2nr(c))
    endw

    let c='A'
    while c <= 'Z'
      exec "set <A-".c.">=\e".c
      "exec "imap \e".c." <A-".c.">"
      let c = nr2char(1+char2nr(c))
    endw
  endif
endif
"if has('nvim')
  ""nnoremap <C-H> <C-w>5<
  "map <silent> <bs> <C-w>5<
  "nnoremap <M-H> <C-w>H

  "nnoremap <c-j> <c-w>5-
  "nnoremap <m-j> <c-w>j

  "nnoremap <C-K> <C-w>5+
  "nnoremap <M-K> <C-w>K

  "nnoremap <C-L> <C-w>5>
  "nnoremap <M-L> <C-w>L
"else
  "set ttymouse=xterm2


  nnoremap <M-H> <C-w>H
  nnoremap <M-J> <C-w>J
  nnoremap <M-K> <C-w>K
  nnoremap <M-L> <C-w>L
  

  nnoremap <M--> <C-w>5-

  nnoremap <M-+> <C-w>5+

  nnoremap <M-<> <C-w>5<

  nnoremap <M->> <C-w>5>
"endif


inoremap <M-u> <Esc>vbUea
inoremap <M-l> <Esc>vbuea


inoremap <M-o> <C-x><C-o>
inoremap <M-k> <C-x><C-]>
inoremap <M-]> <C-x><C-]>
"inoremap <M-Space> <C-x><C-o>

"set <F21>=^[^O
""inoremap <F21> :echo 'hello'<CR>
"inoremap <F21> <C-x><C-]>
"inoremap <M-k> <C-x><C-]>

"-----------split management----------------------- 
set nosplitbelow
set splitright

" move between splits by holding ctrl
"nnoremap <silent> <c-h> <c-w>h
"nnoremap <silent> <c-j> <c-w>j
"nnoremap <silent> <c-k> <c-w>k
"nnoremap <silent> <c-l> <c-w>l

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
"set t_Co=256
"let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : ''  }
"set background=dark
"colorscheme jellybeans
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
inoremap jk <Esc>
"nnoremap <Leader>w <C-w>

"buffer
"nnoremap <Return> :w<CR>
nnoremap <M-s> :w<CR>
nnoremap <Leader>q :bdelete<CR>
nnoremap <Leader>x :call ConfirmBDeleteBang()<CR>
nnoremap <Leader>ns :new<CR>
nnoremap <Leader>nv :vnew<CR>

"window stuff
nnoremap <Leader>w <C-w>
nnoremap <M-c> <C-w>c
nnoremap <M-q> :bdelete<CR>
"nnoremap <Leader>wh <C-w>H
"nnoremap <Leader>wj <C-w>J
"nnoremap <Leader>wk <C-w>K
"nnoremap <Leader>wl <C-w>L
"nnoremap <Leader>wo <C-w>o
"nnoremap <Leader>wc <C-w>c
"nnoremap <Leader>ww <C-w>w
"nnoremap <Leader>ws :split <CR>
"nnoremap <Leader>wv :vsplit <CR>
"nnoremap <Leader>wa gg<S-v><S-g> " highlight all


"tabs
"noremap <Leader><s-tab> :tabprevious<CR>
"noremap <Leader><tab> :tabnext<CR>

" syntax on
"noremap <Leader>.s :syntax on<CR>

nnoremap <Leader>oc :copen <CR>

nnoremap <M-f> :grep 

" ack.vim
let g:ack_use_dispatch = 1
nnoremap <Leader>8 :Ack 
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

nnoremap <M-a> :Ack 

"easymotion settings
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_smartcase = 1
map <silent> / <Plug>(easymotion-sn)
omap <silent> / <Plug>(easymotion-tn)
map  <Leader>; <Plug>(easymotion-bd-f)
"nmap <Leader>; <Plug>(easymotion-overwin-f)
"map  <M-Space> <Plug>(easymotion-bd-f)
"nmap <M-Space> <Plug>(easymotion-overwin-f)

"map <Leader>' <Plug>(easymotion-bd-f)
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
nnoremap <Leader>gc :Commits<CR>

" gitgutter
let g:gitgutter_signs = 0

" netrw
nnoremap <Leader>1 :edit .<CR>
nnoremap <Leader>2 :Explore<CR>
nnoremap <Leader>3 :Sexplore<CR>
nnoremap <Leader>4 :Vexplore<CR>

" statusline settings
"set statusline=%t         " Path to the file
"set statusline+=\ -\      " separator
"set statusline+=FileType: " label
"set statusline+=%y        " Filetype of the file
"set statusline+=\ -\      " separator
"set statusline+=%{fugitive#statusline()}
"set statusline+=%=        " switch to right side
"set statusline+=%#warningsmsg#
"set statusline+={SyntasticStatuslineFlag()}
"set statusline+=%*

" syntastic settings
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
"let g:syntastic_php_phpmd_args = 'text unusedcode'
let g:syntastic_php_phpcs_args = '--standard=~/phpcsconfig.xml'
let g:syntastic_mode_map = {
    \ "mode": "active",
    \ "active_filetypes": ["javascript", "python", "ruby", "php"],
    \ "passive_filetypes": ["html", "blade"] }

nnoremap <leader>.sc :SyntasticCheck<CR>

"let g:syntastic_elixir_checkers = ['elixir']

" table mode
let g:table_mode_corner = "|"
let g:table_mode_map_prefix = '<Leader>t'

" dispatch
nnoremap <Leader>dp :Dispatch <CR>
nnoremap <Leader>ds :Start <CR>
vnoremap <Leader>dp y:call DispatchCommand(@@)<CR>
vnoremap <Leader>ds y:call DispatchCommand(@@, "Start")<CR>

" tags
"nnoremap <Leader>lt :tag<space>
"nnoremap <Leader>t :tselect 
set tags+=tags.vendor

" ctrlp settings
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_max_files=0
let g:ctrlp_by_filename=0
let g:ctrlp_working_path_mode = '0'
"let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir', 'rtscript',
                          "\ 'undo', 'line', 'changes', 'mixed', 'bookmarkdir']
"let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
"let g:ctrlp_custom_ignore = 'node_modules'
let g:ctrlp_custom_ignore = '\v[\/](vendor|node_modules|target|dist)|(\.(swp|ico|git|svn))$'
let g:ctrlp_show_hidden = 1
"let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:50,results:30'
let g:ctrlp_buftag_types = {
    \ 'php'        : '--fields=K --PHP-kinds=mctdfip --languages=php',
  \ }

if has('nvim')
"if 1
  "nnoremap <Leader>f :Files<CR>
  nnoremap <Leader>f :CtrlP<CR>
  "nnoremap <Leader>lmru :CtrlPMRUFiles<CR>
  nnoremap <Leader>b :Buffers<CR>
  "nnoremap <Leader>b :CtrlPBuffer<CR>
  "nnoremap <Leader>b :Unite buffer -start-insert -smartcase -direction=botright<CR>
  "nnoremap <Leader>b :Unite buffer -start-insert -ignorecase<CR>
  nnoremap <Leader>k :MyTagList<CR>
  "nnoremap <Leader>k :CtrlPTag<CR>
  "nnoremap <Leader>k :Unite tag -start-insert -ignorecase -vertical-preview<CR>
  "nnoremap <Leader>a :Unite tag -start-insert -ignorecase<CR>
  nnoremap <Leader>l :CtrlPBufTag<CR>
  "nnoremap <Leader>l :MyBufferTags<CR>
  nnoremap <Leader>a :CtrlPBufTagAll<CR>
  "nnoremap <Leader>ld :CtrlPDir<CR>
else
  call unite#filters#matcher_default#use(['matcher_fuzzy'])

  nnoremap <Leader>f :CtrlP<CR>
  nnoremap <Leader>b :CtrlPBuffer<CR>
  "nnoremap <Leader>b :Unite buffer -start-insert -ignorecase -direction=botright<CR>
  nnoremap <Leader>k :CtrlPTag<CR>
  "nnoremap <Leader>k :Unite tag -start-insert -ignorecase -vertical-preview -direction=botright<CR>
  nnoremap <Leader>l :CtrlPBufTag<CR>
  nnoremap <Leader>a :CtrlPBufTagAll<CR>
endif


" delimitmate settings
let g:delimitMate_expand_cr=1
let g:delimitMate_expand_space=1

" airline settings
"let g:airline_theme='base16'
let g:airline_theme='gruvbox'
if !has('gui_running')
  let g:airline_left_sep=''
  let g:airline_right_sep=''
endif
let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"
"let g:airline#extensions#tabline#enabled = 1
"let g:airline#extensions#tabline#show_buffers = 1

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
let g:jedi#popup_on_dot = 1
let g:jedi#auto_close_doc = 0
let g:jedi#show_call_signatures = 2
let g:jedi#popup_select_first = 1

" vim-rest-console
let g:vrc_trigger = '<Leader>.mr'


nnoremap <Leader>.p :set paste!<CR>
nnoremap <Leader>.ev :e $MYVIMRC<CR>
nnoremap <Leader>.sv :source $MYVIMRC<CR>
nnoremap <Leader>.egt :e ~/todo.txt<CR>
nnoremap <Leader>.ept :e .derick/todo.txt<CR>
nnoremap <Leader>.epn :e .derick/notes.md<CR>

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
        \ 'y:interfaces',
        \ 'z:classes',
        \ 'w:constants',
        \ 'm:methods',
        \ 'x:functions',
        \ 't:traits',
        \ 'p:properties',
        \ 'r:static_properties',
        \ 's:static_methods',
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

"" youcompleteme
"let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
"let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']

" ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsListSnippets="<c-l>"
let g:UltiSnipsJumpForwardTrigger="<m-n>"
let g:UltiSnipsJumpBackwardTrigger="<m-p>"
"let g:UltiSnipsEditSplit="vertical"
nnoremap <Leader>.es :UltiSnipsEdit<CR>
nnoremap <Leader>.eas :e ~/.vim/Ultisnips/all.snippets<CR>

"inoremap <c-l> <esc>:Unite ultisnips -start-insert<CR>
inoremap <M-s> <c-o>:Snippets<CR>
"inoremap <M-s> <c-o>:Unite ultisnips -start-insert<CR>

nnoremap <Leader>.os :syntax on<CR>

nnoremap <Leader>.sc :SyntasticCheck<CR>
nnoremap <Leader>.st :SyntasticToggleMode<CR>




vnoremap <Leader>be y:call Base64Encode(@@)<CR>
vnoremap <Leader>bd y:call Base64Decode(@@)<CR>

nnoremap <Leader>,jt :%!python -m json.tool<CR>


nnoremap <Leader>,a yiw:call AckSearchWord(@@, '.')<CR>


imap <c-x><c-f> <plug>(fzf-complete-path)



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

" neovim
" deoplete.
if has('nvim')
  let g:deoplete#enable_at_startup = 1
endif

" alchemist.vim
"let g:alchemist_iex_term_split = 'vsplit'

if has('gui_macvim') && has('gui_running')
  let g:my_tmux_pane= 'runner'
else
  let g:my_tmux_pane= 'runner'
endif

function! SendToTmuxPane()
  exe "normal V\<C-[>"
  exe "silent '<,'>Twrite " . g:my_tmux_pane
endfunction

inoremap <silent> <M-t> <C-o>:call SendToTmuxPane()<CR>
vnoremap <M-t> :\<C-u>execute "'<,'>Twrite " . g:my_tmux_pane <CR>
nnoremap <silent> <M-t> :call SendToTmuxPane()<CR>
nnoremap <silent> <M-x> :Tmux kill-window -t runner<CR>

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

"function! RunPhpSpecOnBuffer(buffer_name)
  "" TODO: don't hardcode console:runner.1
  ""       maybe use a global config or something
  ""exe "Tmux send-keys -t console:runner.1 'clear; phpspec run " . fnameescape(a:buffer_name) . "' Enter"
  "exe "Start phpspec run " . fnameescape(a:buffer_name) . " && read"
"endfunction

function! RunPhpSpecOnBuffer(buffer_name)
  " TODO: don't hardcode console:runner.1
  "       maybe use a global config or something
  "exe "Tmux send-keys -t console:runner.1 'clear; phpspec run " . fnameescape(a:buffer_name) . "' Enter"
  let l:project_dir = fnamemodify('.', ':p')
  let l:phpspec_exe = fnamemodify('vendor/bin/phpspec run', ':p')
  let l:file = expand('%:p')
  let l:cmd = 'cd ' . l:project_dir . ' && clear && ' . l:phpspec_exe . ' ' . l:file
  "exe "Start phpspec run " . fnameescape(a:buffer_name) . " && read"
  exe "Tmux neww -t runner"
  exe "Tmux send-keys -t runner '" . l:cmd . "' Enter"
endfunction

function! RunArtisanTinkerInProjectRootDirectory()
  let l:project_dir = fnamemodify('.', ':p')
  let l:cmd = 'cd ' . l:project_dir . ' && php artisan tinker'
  exe "Tmux neww -t runner '" . l:cmd . "'"
endfunction

function! RunBehatOnFile()
  let l:project_dir = fnamemodify('.', ':p')
  let l:behat_exe = fnamemodify('vendor/bin/behat', ':p')
  let l:file = expand('%:p')
  let l:cmd = 'cd ' . l:project_dir . ' && clear && ' . l:behat_exe . ' --append-snippets ' . l:file
  exe "Tmux neww -t runner"
  exe "Tmux send-keys -t runner '" . l:cmd . "' Enter"
endfunction

"-------------Auto-Commands--------------"

augroup filetype_dosini
  autocmd!
  autocmd BufRead,BufNewFile php-fpm.conf set syntax=dosini
  autocmd BufRead,BufNewFile php.ini set syntax=dosini
  autocmd BufRead,BufNewFile www.conf set syntax=dosini
augroup END

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

" cucumber
augroup my_cucumber
  autocmd!
  autocmd FileType cucumber setlocal shiftwidth=2 tabstop=2 expandtab softtabstop=2
  "autocmd FileType cucumber nnoremap <buffer> <localleader>rb :Start behat % && read<CR>
  autocmd FileType cucumber nnoremap <buffer> <localleader>rb :call RunBehatOnFile()<CR>
augroup END

" php
augroup my_php
  autocmd!
  " quickly create new php buffers
  autocmd FileType php nnoremap <buffer> <localleader>nv :exe ":vnew \| setfiletype php"<CR>

  " phpspec
  autocmd BufRead,BufNewFile,BufEnter *Spec.php UltiSnipsAddFiletypes php-phpspec
  autocmd FileType php setlocal shiftwidth=2 tabstop=2 expandtab softtabstop=2
  autocmd FileType php setlocal tags+=~/tags/tags.php
  autocmd FileType php nnoremap <buffer> <localleader>mtp :Dispatch create-php-ctags.sh<CR>
  autocmd FileType php nnoremap <buffer> <localleader>mtv :Dispatch create-php-vendor-tags.sh<CR>

  " run behat contexts
  "autocmd FileType php nnoremap <localleader>rb :VimuxRunCommand('clear; behat -f progress') <CR>
  "autocmd FileType php nnoremap <localleader>rb :Tmux send-keys -t console:runner.1 'clear; behat' Enter<CR>
  autocmd FileType php nnoremap <buffer> <localleader>rb :Start behat % && read<CR>
  autocmd FileType php nnoremap <buffer> <localleader>rp :Tmux send-keys -t runner.1 'pwd' Enter<CR>
  
  " run phpspec specs for file or for project
  "autocmd FileType php nnoremap <localleader>rs :VimuxRunCommand('clear; phpspec run ' . bufname('%')) <CR>
  autocmd FileType php nnoremap <buffer> <localleader>rs :call RunPhpSpecOnBuffer(bufname('%')) <CR>
  "autocmd FileType php nnoremap <localleader>rs :Start 'phpspec run ' . (bufname('%') . ' && read' <CR>
  "autocmd FileType php nnoremap <localleader>rps :VimuxRunCommand('clear; phpspec run') <CR>
  autocmd FileType php nnoremap <buffer> <localleader>rps :Start phpspec run && read<CR>

  " run phpunit tests for file or for project
  "autocmd FileType php nnoremap <localleader>rt :VimuxRunCommand('clear; phpunit ' . bufname('%')) <CR>
  "autocmd FileType php nnoremap <localleader>rpt :VimuxRunCommand('clear; phpunit') <CR>
  autocmd FileType php nnoremap <buffer> <localleader>rt :Start phpunit %<CR>
  autocmd FileType php nnoremap <buffer> <localleader>rpt :Start phpunit<CR>

  " laravel
  "autocmd FileType php nnoremap <buffer> <localleader>lat :Tmux splitw 'php artisan tinker'<CR>
  autocmd FileType php nnoremap <buffer> <localleader>lat :call RunArtisanTinkerInProjectRootDirectory()<CR>
  autocmd FileType php nnoremap <buffer> <localleader>t :call RunArtisanTinkerInProjectRootDirectory()<CR>

  " codesniffer
  autocmd FileType php nnoremap <buffer> <localleader>cs :Dispatch phpcs % --standard=~/phpcsconfig.xml<CR>
  autocmd FileType php nnoremap <buffer> <localleader>cbf :Dispatch phpcbf % --standard=~/phpcsconfig.xml<CR>

  " setting xdebug on and off
  autocmd FileType php nnoremap <buffer> <localleader>xon :let $XDEBUG_CONFIG="idekey=PHPSTORM"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>xoff :let $XDEBUG_CONFIG=""<CR>

augroup END

augroup ApiBlueprint
  autocmd!
  autocmd FileType apiblueprint nnoremap <buffer> <localleader>md :Dispatch gulp apidocs<CR>
augroup END

augroup phpNamespaces
  autocmd!
  autocmd FileType php inoremap <buffer> <localleader>a <Esc>:call IPhpInsertUse()<CR>
  autocmd FileType php nnoremap <buffer> <localleader>a :call PhpInsertUse()<CR>
  autocmd FileType php inoremap <buffer> <localleader>q <Esc>:call IPhpExpandClass()<CR>
  autocmd FileType php nnoremap <buffer> <localleader>q :call PhpExpandClass()<CR>
  autocmd FileType php inoremap <buffer> <localleader>.s <Esc>:call PhpSortUse()<CR>
  autocmd FileType php nnoremap <buffer> <localleader>.s :call PhpSortUse()<CR>

  autocmd FileType php nnoremap <buffer> <localleader>emo :Emodel 
  autocmd FileType php nnoremap <buffer> <localleader>vmo :Vmodel 
  autocmd FileType php nnoremap <buffer> <localleader>smo :Smodel 
  autocmd FileType php nnoremap <buffer> <localleader>eev :Eevent 
  autocmd FileType php nnoremap <buffer> <localleader>vev :Vevent 
  autocmd FileType php nnoremap <buffer> <localleader>sev :Sevent 
  autocmd FileType php nnoremap <buffer> <localleader>eex :Eexception 
  autocmd FileType php nnoremap <buffer> <localleader>vex :Vexception 
  autocmd FileType php nnoremap <buffer> <localleader>sex :Sexception 
  autocmd FileType php nnoremap <buffer> <localleader>ero :Eroutes <CR>
  autocmd FileType php nnoremap <buffer> <localleader>vro :Vroutes <CR>
  autocmd FileType php nnoremap <buffer> <localleader>sro :Sroutes <CR>
  autocmd FileType php nnoremap <buffer> <localleader>ek :Ekernel <CR>
  autocmd FileType php nnoremap <buffer> <localleader>vk :Vkernel <CR>
  autocmd FileType php nnoremap <buffer> <localleader>sk :Skernel <CR>
  autocmd FileType php nnoremap <buffer> <localleader>eco :Econtroller 
  autocmd FileType php nnoremap <buffer> <localleader>vco :Vcontroller 
  autocmd FileType php nnoremap <buffer> <localleader>sco :Scontroller 
  autocmd FileType php nnoremap <buffer> <localleader>emid :Emiddleware 
  autocmd FileType php nnoremap <buffer> <localleader>vmid :Vmiddleware 
  autocmd FileType php nnoremap <buffer> <localleader>smid :Smiddleware 
  autocmd FileType php nnoremap <buffer> <localleader>ere :Erequest 
  autocmd FileType php nnoremap <buffer> <localleader>vre :Vrequest 
  autocmd FileType php nnoremap <buffer> <localleader>sre :Srequest 
  autocmd FileType php nnoremap <buffer> <localleader>ej :Ejob 
  autocmd FileType php nnoremap <buffer> <localleader>vj :Vjob 
  autocmd FileType php nnoremap <buffer> <localleader>sj :Sjob 
  autocmd FileType php nnoremap <buffer> <localleader>el :Elistener 
  autocmd FileType php nnoremap <buffer> <localleader>vl :Vlistener 
  autocmd FileType php nnoremap <buffer> <localleader>sl :Slistener 
  autocmd FileType php nnoremap <buffer> <localleader>epo :Epolicy 
  autocmd FileType php nnoremap <buffer> <localleader>vpo :Vpolicy 
  autocmd FileType php nnoremap <buffer> <localleader>spo :Spolicy 
  autocmd FileType php nnoremap <buffer> <localleader>epr :Eprovider 
  autocmd FileType php nnoremap <buffer> <localleader>vpr :Vprovider 
  autocmd FileType php nnoremap <buffer> <localleader>spr :Sprovider 
  autocmd FileType php nnoremap <buffer> <localleader>ecfg :Econfig 
  autocmd FileType php nnoremap <buffer> <localleader>vcfg :Vconfig 
  autocmd FileType php nnoremap <buffer> <localleader>scfg :Sconfig 
  autocmd FileType php nnoremap <buffer> <localleader>emig :Emigration 
  autocmd FileType php nnoremap <buffer> <localleader>vmig :Vmigration 
  autocmd FileType php nnoremap <buffer> <localleader>smig :Smigration 
  autocmd FileType php nnoremap <buffer> <localleader>ese :Eseeder 
  autocmd FileType php nnoremap <buffer> <localleader>vse :Vseeder 
  autocmd FileType php nnoremap <buffer> <localleader>sse :Sseeder 
  autocmd FileType php nnoremap <buffer> <localleader>ed :Edoc 
  autocmd FileType php nnoremap <buffer> <localleader>vd :Vdoc 
  autocmd FileType php nnoremap <buffer> <localleader>sd :Sdoc 
  autocmd FileType php nnoremap <buffer> <localleader>ev :Eview 
  autocmd FileType php nnoremap <buffer> <localleader>vv :Vview 
  autocmd FileType php nnoremap <buffer> <localleader>sv :Sview 
  autocmd FileType php nnoremap <buffer> <localleader>etr :Etransformer 
  autocmd FileType php nnoremap <buffer> <localleader>vtr :Vtransformer 
  autocmd FileType php nnoremap <buffer> <localleader>str :Stransformer 
  autocmd FileType php nnoremap <buffer> <localleader>ete :Etest 
  autocmd FileType php nnoremap <buffer> <localleader>vte :Vtest 
  autocmd FileType php nnoremap <buffer> <localleader>ste :Stest 
  autocmd FileType php nnoremap <buffer> <localleader>ea :A<CR>
  autocmd FileType php nnoremap <buffer> <localleader>sa :AS<CR>
  autocmd FileType php nnoremap <buffer> <localleader>va :AV<CR>
augroup END

augroup phpDocumentor
  autocmd!
  autocmd FileType php nnoremap <localleader>d :call pdv#DocumentWithSnip()<CR>
augroup END

augroup phpCsFixer
  autocmd!
  " TODO: remove phpcsfixer
  "autocmd FileType php nnoremap <localleader>cs :call PhpCsFixerFixFile()<CR>
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
  autocmd FileType typescript nnoremap <buffer> <localleader>d :TsuquyomiDefinition<CR>
  autocmd FileType typescript nnoremap <buffer> <localleader>r :TsuquyomiReferences<CR>
  autocmd FileType typescript nnoremap <buffer> <localleader>c :TsuquyomiRenameSymbolC<CR>
  autocmd FileType typescript nnoremap <buffer> <localleader>b :TsuquyomiGoBack<CR>
  autocmd FileType typescript nnoremap <buffer> <localleader>ef :TsuquyomiGeterr<CR>
  autocmd FileType typescript nnoremap <buffer> <localleader>ep :TsuquyomiGeterrProject<CR>
  autocmd FileType typescript nnoremap <buffer> <localleader>t :YcmCompleter GetType<CR>
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
  autocmd FileType elixir imap <buffer> <M-d> <Esc><s-k> <C-w>pa<C-x><C-o>
augroup END

"inoremap <c-space> <esc>:CtrlPBufTagAll <CR>


"let g:dbext_default_profile_homestead = 'type=MYSQL:user=dev:passwd=dev:srvname=192.168.10.10:dbname=home    stead:host=192.168.10.10:port=3306'
"let g:dbext_default_profile = 'homestead'

