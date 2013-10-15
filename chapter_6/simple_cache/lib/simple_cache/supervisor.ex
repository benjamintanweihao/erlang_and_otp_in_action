defmodule SimpleCache.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, server_name}, __MODULE__, [])
  end

  def start_child(value, lease_time) do
    {:ok, pid} = :supervisor.start_child(server_name, [value, lease_time])
    IO.puts "starting child: #{inspect pid}"
    {:ok, pid}
  end

  def init([]) do
    tree = [ worker(SimpleCache.Element, [], [restart: :temporary, shutdown: :brutal_kill]) ]
    supervise(tree, strategy: :simple_one_for_one, max_seconds: 1, max_restarts: 0)
  end

  def server_name do
    __MODULE__
  end
end
