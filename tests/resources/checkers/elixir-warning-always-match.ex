# Warning in this file is for test purpose.
defmodule AlwaysMatch do
  def func(_) do
    IO.puts 'Flycheck is great!'
  end
  def func(:atom) do            # <--- Warning here
    IO.puts 'Cannot get here.'
  end
end