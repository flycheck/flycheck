# Warning in this file is for test purpose.
defmodule Shadowed do
  def func() do
    a = 1
    fn(^a) -> a end         # <--- Warning here
    a
  end
end