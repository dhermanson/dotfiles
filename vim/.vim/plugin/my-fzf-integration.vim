function! s:line_to_parts(line)
  let parts = split(a:line, '\t\zs')
  let excmd = matchstr(parts[2], '^.*\ze;"\t')
  echom "excmd: " . excmd
  return { 'name': parts[0],
        \  'file': parts[1],
        \  'excmd': excmd,
        \  'type': parts[3] }
endfunction

function! s:handle_selection(lines)
  if len(a:lines) < 2 | return | endif

  let cmd = get({'ctrl-x': 'split',
               \ 'ctrl-v': 'vertical split',
               \ 'ctrl-t': 'tabe'}, a:lines[0], 'e')

  let parts = s:line_to_parts(a:lines[1])

  if cmd ==# 'split'
    execute "split"
    wincmd p
  elseif cmd ==# "vertical split"
    execute "vertical split"
    wincmd p
  elseif cmd ==# "tabe"
    execute "tabe"
  endif

  execute "tjump " . parts['name']

  "execute cmd escape(parts['file'], ' %#\')

  "let [magic, &magic] = [$magic, 0]
  "execute parts['excmd']
  "let $magic = magic
endfunction

    "\ 'source': 'tail -n +7 ~/tags/tags.php tags',
function! s:taglist()
  call fzf#run({
    \ 'source':  'cat '.join(map(tagfiles(), 'fnamemodify(v:val, ":S")')).
    \            '| grep -v ^!',
    \ 'options': '-n 1 --expect=ctrl-t,ctrl-v,ctrl-x',
    \ 'sink*': function('s:handle_selection'),
    \ 'down':    '50%' })
endfunction

command! MyTagList call s:taglist()
