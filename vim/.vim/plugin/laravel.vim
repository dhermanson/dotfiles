function! GoToBladeView()
  ruby <<EOF
class GoToViewUnderCursorCommand

  def initialize(window, buffer)
    @window = window
    @buffer = buffer
  end

  def execute 
    row, col = @window.cursor
    line = @buffer.line
    quote = /('|")/

    until col == 0 or quote.match(line[col])
      col -= 1
    end

    # check if cursor is on an apostrophe or quotation mark
    if quote.match(line[col])
      Vim.command(%Q(let saved_unnamed_register = @@))
      if /'/.match(line[col])
        Vim.command(%Q(execute "normal! yi'"))
      else
        Vim.command(%Q(execute 'normal! yi"'))
      end

      # get the view name
      view_name = Vim.evaluate(%Q(shellescape(@@)))

      # reset the pre-existing saved unnamed register
      Vim.command(%Q(let @@ = saved_unnamed_register))

      # convert dots to forward slashes
      projectionist_view_path = view_name.gsub(/\./, '/')

      # jump to view using projectionist
      Vim.command("Eview #{projectionist_view_path}".gsub(quote, ""))
    end

  end

end

window = Vim::Window.current
buffer = Vim::Buffer.current
GoToViewUnderCursorCommand.new(window, buffer).execute()
EOF
endfunction

