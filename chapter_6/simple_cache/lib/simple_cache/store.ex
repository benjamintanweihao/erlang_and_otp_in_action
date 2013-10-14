defmodule SimpleCache.Store do
  def table_id do
    __MODULE__
  end

  def init do 
    :ets.new(table_id, [:public, :named_table])
    :ok
  end

  def insert(key, pid) do
    :ets.insert(table_id, {key, pid})
  end

  def lookup(key) do
    case :ets.lookup(table_id, key) do
      [{key, pid}] -> {:ok, pid}
      []           -> {:error, :not_found}
    end
  end

  def delete(pid) do
    :ets.match_delete(table_id, {'_', pid})
  end
end
