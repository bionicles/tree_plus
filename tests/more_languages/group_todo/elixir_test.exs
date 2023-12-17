# elixir_test.exs
defmodule Person do
  defstruct name: ""
end

defmodule HelloWorld do
  def hello(person) do
    IO.puts "Hello, #{person.name}!"
  end
end
