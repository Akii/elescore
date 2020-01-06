defmodule Elescore.Store do
  @moduledoc """
  Public API for accessing the event store.
  """

  alias Elescore.Store.{Persistence, Subscription}

  @spec append(atom(), map(), map()) :: :ok
  def append(stream_name, payload, metadata \\ %{}) do
    Persistence.append(stream_name, payload, metadata)
  end

  @spec subscribe(list(atom()), integer()) :: {:ok, pid()} | :error
  def subscribe(stream_names, batch_size \\ 1000) do
    Subscription.subscribe(stream_names, batch_size)
  end
end
