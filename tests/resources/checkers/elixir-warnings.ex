# Warnings from Elixir

defmodule Shadowed do
  def func() do
    a = 1
    fn(^a) -> a end
  end
end

defmodule AlwaysMatch do
  def func(_) do
    IO.puts "Flycheck is great!"
  end
  def func(:atom) do
    IO.puts "Cannot get here."
  end
end
