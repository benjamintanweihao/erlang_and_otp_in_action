defmodule TcpRpc.Server do
  use GenServer.Behaviour 

  ##########
  # HEADER #
  ##########

  # Constants
  def server_name, do: __MODULE__
  def default_port, do: 1055
  
  # Holds the live state of the server process
  defrecord State, port: nil, lsock: nil, request_count: 0

  #######
  # API #
  #######

  def start_link(port) do
    # the :local means that it is a Singleton process. That means only one
    # instance on one node
    :gen_server.start_link({:local, server_name}, __MODULE__, [port], [])
  end

  def start_link do
    :gen_server.start_link({:local, server_name}, __MODULE__, [default_port], [])
  end

  def get_count do
    :gen_server.call(server_name, :get_count)
  end

  def stop do
    :gen_server.cast(server_name, :stop)
  end

  #############
  # Callbacks #
  #############

  def init([port]) do
    {:ok, lsock} = :gen_tcp.listen(port, [{:active, :true}])
    {:ok, State.new(port: port, lsock: lsock), 0}
  end

  def handle_call(:get_count, _from, state) do
    {:reply, {:ok, state.request_count}, state}
  end
  
  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end 

  def handle_info({:tcp, socket, raw_data}, state) do
    do_rpc(socket, raw_data)
    {:noreply, state.update(request_count: state.request_count + 1)}
  end 

  def handle_info(:timeout, state) do
    {:ok, _sock} = :gen_tcp.accept(state.lsock)
    {:noreply, state}
  end

  def terminate(_reason, _state) do
    :ok
  end

  def code_change(_old_version, state, _extra) do
    {:ok, state}
  end

  ######################
  # Internal Functions #
  ######################

  def do_rpc(socket, raw_data) do
    try do
      {m, f, a} = split_out_mfa(raw_data)
      result = apply(m, f, a)
      :gen_tcp.send(socket, :io_lib.fwrite("~p~n", [result]))
    rescue
      _ -> 
        :gen_tcp.send(socket, :io_lib.fwrite("~p~n", ["error!"]))
    end
  end

  def split_out_mfa(raw_data) do
    mfa = :re.replace(raw_data, "\r\n$", "", [{:return, :list}])
    {:match, [m, f, a]} = :re.run(mfa, "\\:(\\w+)\\.(\\w+)\\((.*)\\)$", 
                            [{:capture, [1,2,3], :list}, :ungreedy])

    {list_to_atom(m), list_to_atom(f), args_to_terms(a)}
  end

  # NOTE: Too lazy to figure this out. So I'll assume every RPC has
  #       no arguments.
  def args_to_terms(_raw_args) do
    []
  end
  
end
