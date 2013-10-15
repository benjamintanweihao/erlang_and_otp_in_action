defmodule SimpleCache do
  use Application.Behaviour

  def mass_insert do
    Enum.each(1..1_000_000, fn(x) -> insert(x, x*x) end)
  end

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    IO.puts "Starting SimpleCache ..."

    SimpleCache.Store.init
    case SimpleCache.Supervisor.start_link do
      {:ok, pid} -> 
        IO.puts "pid of supervisor is #{inspect pid}" 
        {:ok, pid}
      other -> 
        {:error, other}
    end
  end

  def stop(_state) do
    :ok
  end

  def insert(key, value) do
    IO.inspect key
    case SimpleCache.Store.lookup(key) do
      {:ok, pid} ->
        SimpleCache.Element.replace(pid, value)
      {:error, _} ->  
        {:ok, pid} = SimpleCache.Element.create(value)
        SimpleCache.Store.insert(key, pid)
    end
  end

  def lookup(key) do
    try do
      {:ok, pid}   = SimpleCache.Store.lookup(key)
      {:ok, value} = SimpleCache.Element.fetch(pid) 
      {:ok, value}
    rescue
      _ -> {:error, :not_found}
    end
  end

  def delete(key) do
    case SimpleCache.Store.lookup(key) do
      {:ok, pid} -> 
        SimpleCache.Element.delete(pid)
      {:error, _reason} -> 
        :ok
    end
  end

end
