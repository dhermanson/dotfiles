function! s:align_lists(lists)
  let maxes = {}
  for list in a:lists
    let i = 0
    while i < len(list)
      let maxes[i] = max([get(maxes, i, 0), len(list[i])])
      let i += 1
    endwhile
  endfor
  for list in a:lists
    call map(list, "printf('%-'.maxes[v:key].'s', v:val)")
  endfor
  return a:lists
endfunction

function! s:btags_source()
  let lines = map(split(system(printf(
    \ 'ctags -f - --sort=no --excmd=number --PHP-kinds=tzxyprms --language-force=%s %s',
    \ &filetype, expand('%:S'))), "\n"), 'split(v:val, "\t")')
  if v:shell_error
    throw 'failed to extract tags'
  endif
  return map(s:align_lists(lines), 'join(v:val, "\t")')
endfunction

function! s:btags_sink(line)
  execute split(a:line, "\t")[2]
  execute "normal! zz"
endfunction

function! s:handle_selection(lines)
  echo a:lines
  if len(a:lines) < 2 | return | endif

  let cmd = get({'ctrl-x': 'split',
               \ 'ctrl-v': 'vertical split',
               \ 'ctrl-t': 'tabe'}, a:lines[0], 'e')

  echo cmd

  "let parts = s:line_to_parts(a:lines[1])

  "execute cmd escape(parts['file'], ' %#\')

  "let [magic, &magic] = [$magic, 0]
  "execute parts['excmd']
  "let $magic = magic

  "let search_pattern = "normal! " . parts['pattern'] . "<cr>"
endfunction

    "\ 'options': '+m -d "\t" --with-nth 1,4.. -n 1 --tiebreak=index',
function! s:btags()
  try
    call fzf#run({
    \ 'source':  s:btags_source(),
    \ 'options': '-n 1 -d "\t" --with-nth 1,4.. --expect=ctrl-t,ctrl-v,ctrl-x',
    \ 'down':    '40%',
    \ 'sink*':    function('s:handle_selection')})
  catch
    echohl WarningMsg
    echom v:exception
    echohl None
  endtry
endfunction

command! MyBufferTags call s:btags()
