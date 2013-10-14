defmodule TcpRpc.Mixfile do
  use Mix.Project

  def project do
    [ app: :tcp_rpc,
      version: "0.0.1",
      elixir: "~> 0.10.3-dev",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [:tcp_rpc], 
      mod: {TcpRpc, []} ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
