defmodule Elescore.Store do
  @moduledoc """
  Public API for accessing the event store.
  """

  alias Elescore.Store.{Persistence, Subscription}

  @spec append(atom(), map(), map()) :: :ok
  def append(stream_name, payload, metadata \\ %{}) do
    Persistence.append(stream_name, payload, metadata)
  end

  @spec subscribe(atom(), integer()) :: {:ok, pid()} | :error
  def subscribe(stream_name, batch_size \\ 10) do
    Subscription.subscribe(stream_name, batch_size)
  end
end
