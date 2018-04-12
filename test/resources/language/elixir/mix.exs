defmodule FlycheckTests.Mixfile do
  use Mix.Project

  def project do
    [app: :flycheck_tests,
     version: "0.0.1",
     deps: deps()]
  end

  def deps do
    [{:credo, "~> 0.9.2"}]
  end
end
