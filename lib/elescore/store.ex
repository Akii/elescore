defmodule Elescore.Store do
  @moduledoc """
  Public API for accessing the event store.
  """

  alias Elescore.Store.{Persistence, Subscription}

  @spec append(atom(), atom(), map(), map()) :: :ok
  def append(stream_name, type, payload, metadata \\ %{}) do
    Persistence.append(stream_name, type, payload, metadata)
  end

  @spec subscribe(atom(), integer()) :: {:ok, pid()} | :error
  def subscribe(stream_name, batch_size \\ 10) do
    Persistence.subscribe(stream_name, batch_size)
  end

  @spec get_next_events(pid) :: {:ok, list()}
  def get_next_events(subscription) do
    Subscription.get_next_events(subscription)
  end

  @spec unsubscribe(pid) :: :ok | :error
  def unsubscribe(subscription) do
    Subscription.unsubscribe(subscription)
  end
end
