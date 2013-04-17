# Warning in this file is for test purpose.
defmodule Obsolete do
  def some_fun(alist) when list(alist) do        # <--- Warning here
    IO.inspect alist
  end
end