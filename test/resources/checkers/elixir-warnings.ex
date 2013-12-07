# A warning from Elixir

defmodule AlwaysMatch do
  def func(_) do
    IO.puts "Flycheck is great!"
  end
  def func(:atom) do
    IO.puts "Cannot get here."
  end
end
