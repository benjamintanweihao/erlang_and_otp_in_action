defmodule TcpRpc.Supervisor do
  use Supervisor.Behaviour

  def server_name do
    __MODULE__
  end

  def start_link do
    :supervisor.start_link({:local, server_name}, __MODULE__, [])
  end 

  def init([]) do
    children = [ worker(TcpRpc.Server, [], restart: :permanent) ]
    supervise children, strategy: :one_for_one
  end
end
