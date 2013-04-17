# Warning in this file is for test purpose.
defmodule Obsolete do
  def some_fun(list) when list(list) do        # <--- Warning here
    IO.inspect list
  end
end