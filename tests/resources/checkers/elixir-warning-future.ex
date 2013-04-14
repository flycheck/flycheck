# A warning that will only be emitted in upcoming Elixir 0.8.2

defmodule Obsolete do
  def some_fun(alist) when list(alist) do
    IO.inspect alist
  end
end