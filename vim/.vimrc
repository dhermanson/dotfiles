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
"nnoremap <BS> :e 

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
set cursorline

" no bells
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

let base16colorspace=256  " Access colors present in 256 colorspace"
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors"
set background=dark
"colorscheme base16-tomorrow
"colorscheme base16-twilight
"colorscheme base16-chalk
"colorscheme base16-ashes
"colorscheme base16-ocean

"" base 16 color tweaks
"highlight VertSplit ctermbg=0
"highlight LineNr ctermbg=0
"highlight CursorLineNr ctermbg=0
"highlight GitGutterAdd ctermbg=0
"highlight GitGutterChange ctermbg=0
"highlight GitGutterDelete ctermbg=0
"highlight GitGutterChangeDelete ctermbg=0
"highlight GitGutterAddLine ctermbg=0
"highlight GitGutterChangeLine ctermbg=0
"highlight GitGutterDeleteLine ctermbg=0
"highlight GitGutterChangeDeleteLine ctermbg=0

let g:gruvbox_italic=0
let g:gruvbox_invert_signs=1
"let g:gruvbox_contrast_dark='soft'
"let g:gruvbox_contrast_light='soft'
colorscheme gruvbox
highlight SignColumn ctermbg=235
highlight VertSplit ctermbg=235
"highlight Comment cterm=italic

"colorscheme jellybeans

vnoremap <C-g> <esc>
cnoremap <C-g> <C-c>

"nnoremap <silent> <C-e> $
"nnoremap <silent> <C-a> ^

"inoremap <silent> <C-e> <esc>$a
"inoremap <silent> <C-a> <esc>^i


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
"nnoremap ; :
inoremap jk <Esc>
"nnoremap <Leader>w <C-w>

"buffer
"nnoremap <Return> :w<CR>
"nnoremap <M-s> :w<CR>
nnoremap <Leader>q :bdelete<CR>
nnoremap <Leader>x :call ConfirmBDeleteBang()<CR>
nnoremap <Leader>ns :new<CR>
nnoremap <Leader>nv :vnew<CR>

"window stuff
"nnoremap <Leader>w :NERDTreeToggle<CR>
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
map <Leader>; <Plug>(easymotion-bd-f)
map s <Plug>(easymotion-bd-f)
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
let g:netrw_liststyle=3
"let g:netrw_list_hide= '^.*/tmp/.*$,^.*\.so$,^.*\.swp$,^.*\.zip$,^.*\.class$,^\.\.\=/\=$'
"let g:netrw_list_hide= '.*\.swp$,.*/$' 
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

" editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" neomake
let g:neomake_open_list=2
let g:neomake_verbose=0
augroup my_neomake
  au!
  autocmd! BufWritePost * Neomake
augroup END
let g:neomake_php_enabled_makers = ["php"] " php, phpstan, phpcs, phpmd, phplint
  "let g:neomake_php_enabled_makers = ["php", "phpstan"] " php, phpstan, phpcs, phpmd, phplint
"let g:neomake_php_phpcs_args = '--standard=~/phpcsconfig.xml'
"let g:neomake_php_phpcs_maker = {
    "\ 'args': '--standard=~/phpcsconfig.xml',
    "\ 'errorformat':
      "\ '%-GFile\,Line\,Column\,Type\,Message\,Source\,Severity%.%#,'.
      "\ '"%f"\,%l\,%c\,%t%*[a-zA-Z]\,"%m"\,%*[a-zA-Z0-9_.-]\,%*[0-9]%.%#',
"\ }

      "\ 'mapexpr': 'neomake_bufdir . "/" . neomake_bufname . v:val'
"function! ProcessEntry(entry)
  ""a:entry.file = call bufname('%')
  "a:entry.type = 'W'
"endfunction
function! ProcessEntry(entry)
  let a:entry.type = 'E'
  "for ent in keys(a:entry)
    "echom a:entry[ent]
    "endfor
endfunction

      "\ 'mapexpr': '"File:app/Models/Associate.php:37:This is a stupid error"',
let g:neomake_php_phpstan_maker = {
      \ 'args': ['analyse', '--no-ansi', '--no-progress', '--autoload-file=vendor/autoload.php'],
      \ 'errorformat':
      \   '%-G\ -%.%#,'.
      \   '%-G\ \[%.%#,'.
      \   '%EFile:%f:\ \ %l\ %m,'.
      \   '%E\ \ %l\ %m,'.
      \   '%-G%.%#',
      \ 'mapexpr': '"File:" . neomake_bufname . ":" . v:val',
      \ 'postprocess': function('ProcessEntry')
      \}

" TODO: swap out all syntastic stuff with neomake
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
let g:syntastic_javascript_checkers = ['eslint', 'jshint']
"let g:syntastic_apiblueprint_checkers = ['drafter']
"let g:syntastic_apiblueprint_drafter_exec = "/usr/local/bin/drafter"
"let g:syntastic_python_python_exec = '/usr/local/bin/python3'
let g:syntastic_python_python_exec = 'python3'
let g:syntastic_python_checkers = ['mypy', 'python']
"let g:syntastic_python_mypy_exec = '/usr/local/bin/mypy'
let g:syntastic_cs_checkers = ['mcs']
"let g:syntastic_typescript_tsc_args = '--module commonjs --target ES5 --experimentalDecorators'
let g:syntastic_typescript_tsc_fname = ''
let g:syntastic_typescript_checkers = ['']
let g:syntastic_php_checkers = ["php"] " php, phpcs, phpmd, phplint
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
let g:table_mode_map_prefix = '<Leader>5'

" dispatch
nnoremap <Leader>dp :Dispatch 
nnoremap <Leader>ds :Start 
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
let g:ctrlp_match_window = 'results:100' " overcome limit imposed by max height
"let g:ctrlp_match_window = 'min:4,max:72'
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

"if has('nvim')
"if 1
  "nnoremap <Leader>f :Files<CR>
  nnoremap <Leader>f :Files<CR>
  "nnoremap <Leader>lmru :CtrlPMRUFiles<CR>
  nnoremap <Leader>b :Buffers<CR>
  "nnoremap <Leader>b :CtrlPBuffer<CR>
  "nnoremap <Leader>b :Unite buffer -start-insert -smartcase -direction=botright<CR>
  "nnoremap <Leader>b :Unite buffer -start-insert -ignorecase<CR>
  nnoremap <Leader>k :MyTagList<CR>
  "nnoremap <Leader>k :CtrlPTag<CR>
  "nnoremap <Leader>k :Unite tag -start-insert -ignorecase -vertical-preview<CR>
  "nnoremap <Leader>a :Unite tag -start-insert -ignorecase<CR>
  "nnoremap <Leader>l :CtrlPBufTag<CR>
  nnoremap <Leader>l :MyBufferTags<CR>
  "nnoremap <Leader>a :CtrlPBufTagAll<CR>
  "nnoremap <Leader>ld :CtrlPDir<CR>
"else

  "nnoremap <Leader>f :CtrlP<CR>
  "nnoremap <Leader>b :CtrlPBuffer<CR>
  ""nnoremap <Leader>b :Unite buffer -start-insert -ignorecase -direction=botright<CR>
  ""nnoremap <Leader>k :CtrlPTag<CR>
  "nnoremap <Leader>k :Unite tag -start-insert -ignorecase -vertical-preview -direction=botright<CR>
  "nnoremap <Leader>l :CtrlPBufTag<CR>
  "nnoremap <Leader>a :CtrlPBufTagAll<CR>
"endif

" unite settings
"call unite#filters#matcher_default#use(['matcher_fuzzy'])
"call unite#custom#source('grep', 'max_candidates', 0)

nnoremap <Leader>ug :Unite -buffer-name=grep-results grep<CR>
nnoremap <Leader>ub :Unite -buffer-name=buffers buffer <CR>

nnoremap <Leader>ur :UniteResume<CR>
nnoremap <Leader>un :UniteNext<CR>
nnoremap <Leader>up :UnitePrevious<CR>


" delimitmate settings
let g:delimitMate_expand_cr=1
let g:delimitMate_expand_space=1

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
let g:dbext_default_usermaps = 0
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


"imap <c-x><c-f> <plug>(fzf-complete-path)

"" omnisharp
"let g:OmniSharp_selector_ui = 'ctrlp'
"let g:OmniSharp_server_type = 'roslyn'



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
  "let g:deoplete#omni_patterns = {}
  "let g:deoplete#omni_patterns.java = '[^. *\t]\.\w*'
  "let g:deoplete#auto_completion_start_length = 2
  let g:deoplete#sources = {}
  let g:deoplete#sources['php'] = ['omni', 'tag', 'file', 'ultisnips', 'buffer', 'member']
  let g:deoplete#enable_ignore_case = 1
  let g:deoplete#enable_smart_case = 1
  "let g:deoplete#keyword_patterns = {}
  "let g:deoplete#keyword_patterns['php'] = 'function\w*'
  "let g:deoplete#sources._ = []
  "let g:deoplete#file#enable_buffer_path = 1
  
  "call deoplete#custom#set('ultisnips', 'matchers', ['matcher_fuzzy'])
endif

" alchemist.vim
"let g:alchemist_iex_term_split = 'vsplit'

"if has('gui_macvim') && has('gui_running')
  "let g:my_tmux_pane= 'runner'
"else
  "let g:my_tmux_pane= 'runner'
"endif

function! SendToTmuxPane()
  exe "normal V\<C-[>"
  exe "silent '<,'>Twrite " . g:my_tmux_repl_pane
endfunction

function! KillTmuxRepl()
  if exists('g:my_tmux_repl_pane')
    call system("tmux kill-pane -t " . g:my_tmux_repl_pane)
  endif
endfunction

function! ClearRepl()
  call system("tmux send-keys -t " . g:my_tmux_repl_pane . " 'C-l'")
endfunction

inoremap <silent> <M-t> <C-o>:call SendToTmuxPane()<CR>
vnoremap <M-t> :\<C-u>execute "'<,'>Twrite " . g:my_tmux_repl_pane <CR>
nnoremap <silent> <M-t> :call SendToTmuxPane()<CR>
"nnoremap <silent> <M-x> :Tmux kill-pane -t g:my_tmux_repl_pane<CR>
nnoremap <silent> <M-x> :call KillTmuxRepl()<CR>
"nnoremap <silent> <M-l> :call ClearRepl()<CR>

" slimux
"nnoremap <M-s> :SlimuxREPLSendLine<CR>
"vnoremap <M-s> :\<C-u>execute "'<,'>SlimuxREPLSendSelection" <CR>
"inoremap <silent> <M-s> <C-o>:SlimuxREPLSendLine<CR>

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
  "exe "Tmux neww -t runner"
  "exe "Tmux send-keys -t runner '" . l:cmd . "' Enter"
  "exe "Tmux neww -t runner '" . l:cmd . "'"
  exe "Tmux splitw '" . l:cmd . " ; read'"
endfunction

function! RunArtisanTinkerInProjectRootDirectory()
  let l:project_dir = fnamemodify('.', ':p')
  let l:cmd = 'cd ' . l:project_dir . ' && php artisan tinker'
  exe "Tmux neww -t runner -n artisan '" . l:cmd . "'"
  let g:my_tmux_repl_pane = "runner:artisan.0"
endfunction

function! RunArtisanTinkerInProjectRootDirectoryInTmuxSplit()
  let l:project_dir = fnamemodify('.', ':p')
  let l:cmd = 'cd ' . l:project_dir . ' && php artisan tinker'
  exe "Tmux splitw '" . l:cmd . "'"
  exe "Tmux last-pane"
endfunction

function! RunBehatOnFile()
  let l:project_dir = fnamemodify('.', ':p')
  let l:behat_exe = fnamemodify('vendor/bin/behat', ':p')
  let l:file = expand('%:p')
  let l:cmd = 'cd ' . l:project_dir . ' && clear && ' . l:behat_exe . ' --append-snippets ' . l:file
  exe "Tmux neww -t runner"
  exe "Tmux send-keys -t runner '" . l:cmd . "' Enter"
endfunction

function! GetTmuxSession()
  return systemlist("tmux display-message -p '#S'")[0]
endfunction

function! GetTmuxWindowName()
  return systemlist("tmux display-message -p '#{window_name}'")[0]
endfunction

function! GetTmuxPaneIndex()
  return systemlist("tmux display-message -p '#{pane_index}'")[0]
endfunction

function! GetTmuxPaneId()
  return systemlist("tmux display-message -p '#{pane_id}'")[0]
endfunction

function! GetTmuxPane()
  let l:session = GetTmuxSession()
  let l:window = GetTmuxWindowName()
  let l:pane_index = GetTmuxPaneIndex()
  return l:session . ":" . l:window . "." . l:pane_index
endfunction

function! CreateTmuxSplit()
  call system("tmux splitw")
  "let l:pane = GetTmuxPane()
  let l:pane = GetTmuxPaneId()
  call system("tmux last-pane")
  return l:pane
endfunction

function! CreateTmuxSplitAndRunCommand(command, split)
  let l:cmd = shellescape(a:command)
  call system("tmux splitw " . a:split . " " . l:cmd)
  "let l:pane = GetTmuxPane()
  let l:pane = GetTmuxPaneId()
  call system("tmux last-pane")
  return l:pane
endfunction


function! RunCommandInTmuxPane(pane, command)
  let l:cmd = shellescape(a:command)
  call system("tmux send-keys -t " . a:pane . " " . l:cmd . " Enter")
endfunction

function! RunCommandInSplit(command, split)
  call KillTmuxRepl()
  let l:escaped_cmd = shellescape(a:command)
  let l:project_dir = fnamemodify('.', ':p')
  let l:cmd = 'cd ' . l:project_dir . ' && ' . l:escaped_cmd
  let l:pane = CreateTmuxSplitAndRunCommand(l:cmd, a:split)
  let g:my_tmux_repl_pane = l:pane

  "call RunCommandInTmuxPane(l:pane, l:cmd)
endfunction

function! RunNodeInSplit(split)
  call KillTmuxRepl()
  let l:project_dir = fnamemodify('.', ':p')
  let l:cmd = 'cd ' . l:project_dir . ' && node ~/node_repl.js'
  let l:pane = CreateTmuxSplitAndRunCommand(l:cmd, a:split)
  let g:my_tmux_repl_pane = l:pane

  "call RunCommandInTmuxPane(l:pane, l:cmd)
endfunction

function! RunArtisanTinkerInSplit(split)
  call KillTmuxRepl()
  let l:project_dir = fnamemodify('.', ':p')
  let l:cmd = 'cd ' . l:project_dir . ' && php artisan tinker'
  let l:pane = CreateTmuxSplitAndRunCommand(l:cmd, a:split)
  let g:my_tmux_repl_pane = l:pane

  "call RunCommandInTmuxPane(l:pane, l:cmd)
endfunction

function! CreatePhpSplitAndStartRepl(buffersplit, tmuxsplit)
  execute "normal! :" . a:buffersplit . "\<CR>"
  :setfiletype php
  :call RunArtisanTinkerInSplit(a:tmuxsplit)
  execute "normal! i<?php\<CR>\<CR>"
  :startinsert
endfunction

function! RemoveUnusedPhpUseStatementsForCurrentFile()
  " check if buffer has been modified
  if (&mod)
    "let l:choice = confirm("Save buffer first?", "&Yes\n&No")
    "if l:choice == 1
      execute "w"
    "endif
  endif

  let l:filename = bufname("%")
  let l:cmd = "php-cs-fixer fix " . l:filename . " --fixers=unused_use"
  call system(l:cmd)
  exe "edit!"
endfunction

nnoremap <silent> <leader>rs :call RunCommandInSplit("bash", "-v")<CR>
nnoremap <silent> <leader>rv :call RunCommandInSplit("bash", "-h")<CR>

"-------------Auto-Commands--------------"

augroup filetype_dosini
  autocmd!
  autocmd BufRead,BufNewFile php-fpm.conf set syntax=dosini
  autocmd BufRead,BufNewFile php.ini set syntax=dosini
  autocmd BufRead,BufNewFile www.conf set syntax=dosini
  autocmd BufRead,BufNewFile *.conf set syntax=dosini
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
  "autocmd FileType php nnoremap <buffer> <leader>ff :exe ":CtrlP"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>rus :call RemoveUnusedPhpUseStatementsForCurrentFile()<CR>
  autocmd FileType php nnoremap <buffer> <localleader>fmig :exe ":FZF database/migrations"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>fmid :exe ":FZF app/Http/Middleware"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>fv :exe ":FZF resources/views"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>fj :exe ":FZF resources/assets/js"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>fl :exe ":FZF resources/assets/less"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>fcf :exe ":FZF config"<CR>

  " quickly create new php buffers
  autocmd FileType php nnoremap <buffer> <localleader>nv :exe ":vnew \| setfiletype php"<CR>
  autocmd FileType php nnoremap <buffer> <localleader>ns :exe ":new \| setfiletype php"<CR>


  autocmd FileType php nnoremap <buffer> <localleader><leader>mamo :Dispatch php artisan make:model 
  autocmd FileType php nnoremap <buffer> <localleader><leader>mi<Space> :Dispatch php artisan migrate<CR>
  autocmd FileType php nnoremap <buffer> <localleader><leader><localleader> :Dispatch php artisan 

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
  "autocmd FileType php nnoremap <buffer> <localleader>rps :Start phpspec run && read<CR>
  autocmd FileType php nnoremap <buffer> <localleader>rps :Tmux splitw 'phpspec run ; read'<CR>

  " run phpunit tests for file or for project
  "autocmd FileType php nnoremap <localleader>rt :VimuxRunCommand('clear; phpunit ' . bufname('%')) <CR>
  "autocmd FileType php nnoremap <localleader>rpt :VimuxRunCommand('clear; phpunit') <CR>
  autocmd FileType php nnoremap <buffer> <localleader>rt :Start phpunit % && read<CR>
  autocmd FileType php nnoremap <buffer> <localleader>rpt :Start phpunit && read<CR>

  " laravel
  "autocmd FileType php nnoremap <buffer> <localleader>lat :Tmux splitw 'php artisan tinker'<CR>
  autocmd FileType php nnoremap <buffer> <localleader>lat :call RunArtisanTinkerInProjectRootDirectory()<CR>
  autocmd FileType php nnoremap <buffer> <localleader>tw :call RunArtisanTinkerInProjectRootDirectory()<CR>
  "autocmd FileType php nnoremap <buffer> <localleader>ts :call RunArtisanTinkerInProjectRootDirectoryInTmuxSplit()<CR>
  autocmd FileType php nnoremap <buffer> <silent> <leader>rr :call RunArtisanTinkerInProjectRootDirectory()<CR>
  autocmd FileType php nnoremap <buffer> <silent> <leader>rc :call ClearRepl()<CR>
  autocmd FileType php nnoremap <buffer> <silent><localleader>ts :call CreatePhpSplitAndStartRepl("vnew", "-v")<CR>
  autocmd FileType php nnoremap <buffer> <silent><localleader>tv :call CreatePhpSplitAndStartRepl("new", "-h")<CR>
  autocmd FileType php nnoremap <buffer> <silent> <leader>rs :call RunCommandInSplit("psysh", "-v")<CR>
  autocmd FileType php nnoremap <buffer> <silent> <leader>rv :call RunCommandInSplit("psysh", "-h")<CR>

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

augroup my_java
  autocmd!
  autocmd FileType java setlocal omnifunc=javacomplete#Complete
  autocmd FileType java setlocal tags+=~/tags/java.tags
augroup END

augroup javascript
  autocmd!
  autocmd FileType javascript nnoremap <buffer> <localleader>e :echo "You've opened a javascript file!"<CR>
  autocmd FileType javascript nnoremap <buffer> <localleader>e :echo "You've opened a javascript file!"<CR>
  autocmd FileType javascript nnoremap <buffer> <leader>rs :call RunNodeInSplit("-v")<CR>
  autocmd FileType javascript nnoremap <buffer> <leader>rv :call RunNodeInSplit("-h")<CR>
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
  autocmd FileType ruby nnoremap <buffer> <leader>rs :call RunCommandInSplit("pry", "-v")<CR>
  autocmd FileType ruby nnoremap <buffer> <leader>rv :call RunCommandInSplit("pry", "-h")<CR>
  " rails
  autocmd FileType ruby nnoremap <buffer> <localleader>rr :Rake 
  autocmd FileType ruby nnoremap <buffer> <localleader>rev :Eview<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rem :Emodel<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rec :Econtroller<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rsv :Sview<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rsm :Smodel<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rsc :Scontroller<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rvv :Vview<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rvm :Vmodel<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rvc :Vcontroller<CR>
  autocmd FileType ruby nnoremap <buffer> <localleader>rgc :Rgenerate controller 

  autocmd FileType ruby nnoremap <buffer> <localleader>rev<Space> :Eview 
  autocmd FileType ruby nnoremap <buffer> <localleader>rem<Space> :Emodel 
  autocmd FileType ruby nnoremap <buffer> <localleader>rec<Space> :Econtroller 
  autocmd FileType ruby nnoremap <buffer> <localleader>rsv<Space> :Sview 
  autocmd FileType ruby nnoremap <buffer> <localleader>rsm<Space> :Smodel 
  autocmd FileType ruby nnoremap <buffer> <localleader>rsc<Space> :Scontroller 
  autocmd FileType ruby nnoremap <buffer> <localleader>rvv<Space> :Vview 
  autocmd FileType ruby nnoremap <buffer> <localleader>rvm<Space> :Vmodel 
  autocmd FileType ruby nnoremap <buffer> <localleader>rvc<Space> :Vcontroller 

  autocmd FileType ruby nnoremap <buffer> <localleader>mtp :Dispatch create-ruby-ctags.sh<CR>
augroup END

augroup my_elixir
  autocmd!
  autocmd FileType elixir setlocal tags+=~/tags/tags.elixir
  autocmd FileType elixir imap <buffer> <M-d> <Esc><s-k> <C-w>pa<C-x><C-o>
augroup END

augroup my_vimscript
  autocmd!
  autocmd FileType vim setlocal omnifunc=syntaxcomplete#Complete
augroup END

"augroup my_netrw
    "au!
    ""au VimEnter * sil! au! FileExplorer *
    ""au BufEnter * if s:isdir(expand('%')) | bd | exe 'Ranger' | endif
    "au BufEnter * if s:isdir(expand('%')) | let g:netrw_list_hide= '^.*/tmp/.*$,^.*\.so$,^.*\.swp$,^.*\.zip$,^.*\.class$,^\.\.\=/\=$' | endif
"augroup END

"fu! s:isdir(dir) abort
    "return !empty(a:dir) && (isdirectory(a:dir) ||
                "\ (!empty($SYSTEMDRIVE) && isdirectory('/'.tolower($SYSTEMDRIVE[0]).a:dir)))
"endfu

"inoremap <c-space> <esc>:CtrlPBufTagAll <CR>


"let g:dbext_default_profile_homestead = 'type=MYSQL:user=dev:passwd=dev:srvname=192.168.10.10:dbname=home    stead:host=192.168.10.10:port=3306'
"let g:dbext_default_profile = 'homestead'


" nerdtree
"nnoremap <leader>t :NERDTreeToggle<CR>


function! SetupLaravelProject()
  let g:projectionist_heuristics = {
        \   "artisan": {
        \     "app/*.php": {
        \       "alternate": "tests/{}Test.php"
        \     },
        \     "app/Models/*.php": {
        \       "type": "model",
        \       "template": ["<?php", "", "namespace App\Models;", "", "class {} {", "}"]
        \     },
        \     "app/Events/*.php": {
        \       "type": "event"
        \     },
        \     "app/Exceptions/*.php": {
        \       "type": "exception"
        \     },
        \     "app/Http/routes.php": {
        \       "type": "routes"
        \     },
        \     "app/Http/Kernel.php": {
        \       "type": "kernel"
        \     },
        \     "app/Http/Controllers/*.php": {
        \       "type": "controller"
        \     },
        \     "app/Http/Middleware/*.php": {
        \       "type": "middleware"
        \     },
        \     "app/Http/Requests/*Request.php": {
        \       "type": "request"
        \     },
        \     "app/Jobs/*.php": {
        \       "type": "job"
        \     },
        \     "app/Listeners/*.php": {
        \       "type": "listener"
        \     },
        \     "app/Policies/*.php": {
        \       "type": "policy"
        \     },
        \     "app/Providers/*.php": {
        \       "type": "provider"
        \     },
        \     "app/Repositories/*.php": {
        \       "type": "repository"
        \     },
        \     "app/Services/*.php": {
        \       "type": "service"
        \     },
        \     "config/*.php": {
        \       "type": "config"
        \     },
        \     "database/migrations/*.php": {
        \       "type": "migration"
        \     },
        \     "database/seeds/*Seeder.php": {
        \       "type": "seeder"
        \     },
        \     "resources/assets/docs/*.md": {
        \       "type": "doc"
        \     },
        \     "resources/views/*.blade.php": {
        \       "type": "view"
        \     },
        \     "spec/*Spec.php": {
        \       "type": "spec",
        \       "alternate": "app/{}.php",
        \       "dispatch": "vendor/bin/phpspec run {file}",
        \       "start": "vendor/bin/phpspec run {file}"
        \     },
        \     "tests/*Test.php": {
        \       "type": "test",
        \       "alternate": "app/{}.php",
        \       "dispatch": "vendor/bin/phpunit {file}",
        \       "start": "vendor/bin/phpunit {file}"
        \     },
        \     "phpspec.yml": {
        \       "type": "phpspec.yml",
        \       "template": [
        \         "suites:",
        \         "    main:",
        \         "        namespace: App",
        \         "        psr4_prefix: App",
        \         "        src_path: app",
        \         "        src_path: app"
        \       ]
        \     }
        \   }
        \ }

  nnoremap <leader>emo :Emodel 
  nnoremap <leader>vmo :Vmodel 
  nnoremap <leader>smo :Smodel 
  nnoremap <leader>eev :Eevent 
  nnoremap <leader>vev :Vevent 
  nnoremap <leader>sev :Sevent 
  nnoremap <leader>eex :Eexception 
  nnoremap <leader>vex :Vexception 
  nnoremap <leader>sex :Sexception 
  nnoremap <leader>ero :Eroutes <CR>
  nnoremap <leader>vro :Vroutes <CR>
  nnoremap <leader>sro :Sroutes <CR>
  nnoremap <leader>ek :Ekernel <CR>
  nnoremap <leader>vk :Vkernel <CR>
  nnoremap <leader>sk :Skernel <CR>
  nnoremap <leader>eco :Econtroller 
  nnoremap <leader>vco :Vcontroller 
  nnoremap <leader>sco :Scontroller 
  nnoremap <leader>emid :Emiddleware 
  nnoremap <leader>vmid :Vmiddleware 
  nnoremap <leader>smid :Smiddleware 
  nnoremap <leader>ere :Erequest 
  nnoremap <leader>vre :Vrequest 
  nnoremap <leader>sre :Srequest 
  nnoremap <leader>ej :Ejob 
  nnoremap <leader>vj :Vjob 
  nnoremap <leader>sj :Sjob 
  nnoremap <leader>el :Elistener 
  nnoremap <leader>vl :Vlistener 
  nnoremap <leader>sl :Slistener 
  nnoremap <leader>epo :Epolicy 
  nnoremap <leader>vpo :Vpolicy 
  nnoremap <leader>spo :Spolicy 
  nnoremap <leader>epr :Eprovider 
  nnoremap <leader>vpr :Vprovider 
  nnoremap <leader>spr :Sprovider 
  nnoremap <leader>ecfg :Econfig 
  nnoremap <leader>vcfg :Vconfig 
  nnoremap <leader>scfg :Sconfig 
  nnoremap <leader>emig :Emigration 
  nnoremap <leader>vmig :Vmigration 
  nnoremap <leader>smig :Smigration 
  nnoremap <leader>ese :Eseeder 
  nnoremap <leader>vse :Vseeder 
  nnoremap <leader>sse :Sseeder 
  nnoremap <leader>esp :Espec 
  nnoremap <leader>vsp :Vspec 
  nnoremap <leader>ssp :Sspec 
  nnoremap <leader>ed :Edoc 
  nnoremap <leader>vd :Vdoc 
  nnoremap <leader>sd :Sdoc 
  nnoremap <leader>ev :Eview 
  nnoremap <leader>vv :Vview 
  nnoremap <leader>sv :Sview 
  nnoremap <leader>etr :Etransformer 
  nnoremap <leader>vtr :Vtransformer 
  nnoremap <leader>str :Stransformer 
  nnoremap <leader>ete :Etest 
  nnoremap <leader>vte :Vtest 
  nnoremap <leader>ste :Stest 
  nnoremap <leader>ea :A<CR>
  nnoremap <leader>sa :AS<CR>
  nnoremap <leader>va :AV<CR>
  nnoremap <leader>sp :Dispatch phpspec describe App/
  nnoremap <leader>db :call RunMycli()<CR>
  augroup my_laravel
    autocmd!
    autocmd FileType php nnoremap <buffer> <silent> <leader>rs :call RunArtisanTinkerInSplit("-v")<CR>
    autocmd FileType php nnoremap <buffer> <silent> <leader>rv :call RunArtisanTinkerInSplit("-h")<CR>
augroup END

  function! RunArtisanCommand(cmd)
    let escaped_cmd = "php artisan " . shellescape(a:cmd)

    let run_script = $HOME."/.vim/bin/run_then_close_tmux_window"

    let script_command = run_script . " " . escaped_cmd

    call dispatch#start("tmux neww '" . script_command . "'")
  endfunction

  " make sure dependencies installed...can run in background
  "Tmux neww 'ruby '.$HOME."/.vim/bin/laravel/open_mycli.rb"
  call system("tmux splitw -b -p 5 'ruby " . $HOME . "/.vim/bin/laravel/open_mycli.rb'")
  "call system("tmux splitw -p 20")
  "call system("tmux send-keys 'ruby " . $HOME . "/.vim/bin/laravel/open_mycli.rb' && read")
  call system("tmux last-pane")
  "call dispatch#start('ruby '.$HOME."/.vim/bin/laravel/open_mycli.rb")

  function! RunMycli()
    "let run_script = "ruby ".$HOME."/.vim/bin/laravel/open_mycli.rb"
    "call dispatch#start("tmux neww '".run_script."'")
    ruby <<EOD
fork do
  require 'dotenv'

  Dotenv.load '.env'

  host = ENV['DB_HOST']
  port = ENV['DB_PORT']
  db = ENV['DB_DATABASE']
  user = ENV['DB_USERNAME']
  pass = ENV['DB_PASSWORD']

  system "tmux splitw 'mycli -h #{host} -P #{port} -D #{db} -u #{user} -p #{pass}'"
end
EOD

  endfunction

  function! GetAvailableArtisanCommands()
    let results = system("php artisan list")
    let lines = split(results, "\n")

    let commands = []
    let available_commands_hit = 0

    for line in lines
      if !available_commands_hit
        if line == "Available commands:"
          let available_commands_hit = 1
          continue
        endif
      else
        let cmd = split(line)
        if (len(cmd) > 1)
          let c = cmd[0]
          "let trimmed = substitute(c, '^\s*', '', '') 
          let commands = commands + [c]
        endif
      endif
    endfor

    return commands

  endfunction

  function! RunArtisan()
    let width = +system("tput cols")
    let preview = 'down:75%'

    " TODO: fix this
    let width = 200
    if width > 160
      let preview = 'right:70%'
    endif

    call fzf#run({
        \  'source': GetAvailableArtisanCommands(),
        \  'sink': function('RunArtisanCommand'),
        \  'options': '--ansi -i --preview-window=' . l:preview . ' --preview="php artisan {} --help" --bind alt-j:preview-down,alt-k:preview-up',
        \  })
  endfunction

  nnoremap <silent> <leader>a :call RunArtisan()<CR>
  "nnoremap <silent> <leader>a :call fzf#run({
        "\  'source': GetAvailableArtisanCommands(),
        "\  'sink': function('RunArtisanCommand'),
        "\  'options': '--ansi -i --preview="php artisan {} --help" --bind alt-j:preview-down,alt-k:preview-up',
        "\  })<CR>
endfunction

function! SetupProjectType()
  if filereadable("artisan")
    call SetupLaravelProject()
  endif
endfunction

augroup my_vimenter
  autocmd!
  autocmd VimEnter * call SetupProjectType()
augroup END
