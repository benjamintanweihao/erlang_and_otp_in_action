defmodule SimpleCache.Element do
  use GenServer.Behaviour

  defrecord State, value: nil, lease_time: nil, start_time: nil

  def server_name do
    __MODULE__
  end

  def default_lease_time do
    60 * 60 * 24
  end

  #######
  # API #
  #######

  def start_link(value, lease_time) do
    :gen_server.start_link(__MODULE__, [value, lease_time], [])
  end

  def create(value, lease_time) do
    SimpleCache.Supervisor.start_child(value, lease_time)
  end

  def create(value) do
    SimpleCache.Supervisor.start_child(value, default_lease_time)
  end

  def fetch(pid) do
    :gen_server.call(pid, :fetch)
  end

  def replace(pid, value) do
    :gen_server.cast(pid, {:replace, value})
  end

  def delete(pid) do  
    :gen_server.cast(pid, :delete)
  end

  ############
  # Callback #
  ############

  def init([value, lease_time]) do
    now        = :calendar.local_time
    start_time = :calendar.datetime_to_gregorian_seconds(now)
    {:ok, State.new(value: value, lease_time: lease_time, start_time: start_time), time_left(start_time, lease_time)}
  end

  def time_left(_start_time, :infinity) do
    :infinity
  end

  def time_left(start_time, lease_time) do
    now          = :calendar.local_time
    current_time = :calendar.datetime_to_gregorian_seconds(now)
    time_elapsed = current_time - start_time

    time = lease_time - time_elapsed
    if time <= 0 do
      0
    else
      time * 1000
    end
  end

  def handle_call(:fetch, _from, state) do
    time_left = time_left(state.start_time, state.lease_time)
    {:reply, {:ok, state.value}, state, time_left}
  end 

  def handle_cast({:replace, value}, state) do
    time_left = time_left(state.start_time, state.lease_time)
    {:noreply, state.update(value: value), time_left}
  end

  def handle_cast(:delete, state) do
    {:stop, :normal, state}
  end

  def handle_info(:timeout, state) do
    {:stop, :normal, state}
  end

  def terminate(_reason, _state) do
    SimpleCache.Store.delete(self)
    :ok
  end

  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end









end
