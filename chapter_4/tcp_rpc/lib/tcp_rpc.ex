# Implement the Application Behaviour. This requires the implementation
# of start/2 and stop/1.

defmodule TcpRpc do
  use Application.Behaviour

  # Starts up the supervisor  
  def start(_type, _start_args) do
    case TcpRpc.Supervisor.start_link do
      {:ok, pid} -> 
        {:ok, pid}    
      other ->
        {:error, other}
    end
  end

  def stop(_state) do
    :ok
  end

end
