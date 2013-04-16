# Error and Warnings in this file is for test purpose.

# -----------------------------------------------------------------------------
#                                Warnings
# -----------------------------------------------------------------------------

defmodule Obsolete do
  def some_fun(alist) when list(alist) do        # <--- Warning here
    IO.inspect alist
  end
end

defmodule Shadowed do
  def func() do
    a = 1                                        # <--- Warning here
    fn(^a) -> a end                              # <--- Warning here
  end
end

defmodule AlwaysMatch do
  def func(_) do
    IO.puts "Flycheck is great!"
  end
  def func(:atom) do                             # <--- Warning here
    IO.puts "Cannot get here."
  end
end

# -----------------------------------------------------------------------------
#                                  Errors
# -----------------------------------------------------------------------------

defmodule AnError do
  def error_func do
    puts "Flycheck is great!"                    # <--- Error here
  end
end