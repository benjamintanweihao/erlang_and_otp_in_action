defmodule SimpleCache.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def start_child(value, lease_time) do
    IO.puts "starting child!"
    :supervisor.start_child(server_name, [value, lease_time])
  end

  def init([]) do
    children = [
      # Define workers and child supervisors to be supervised
      worker(SimpleCache.Element, [], [restart: :temporary, shutdown: :brutal_kill])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :simple_one_for_one)
  end

  def server_name do
    __MODULE__
  end
end
