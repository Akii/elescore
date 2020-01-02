defmodule Elescore.Store.Persistence do
  @moduledoc """
  Persistence for Events.
  """

  use GenServer
  alias Elescore.Store.{Event, Subscription}

  def init(events) do
    {:ok, events}
  end

  def start_link(options \\ []) do
    GenServer.start_link(__MODULE__, [], options)
  end

  def append(stream_name, type, payload, metadata \\ %{}) do
    event =
      Event.new(
        UUID.uuid4(),
        type,
        stream_name,
        payload,
        metadata
      )

    GenServer.call(__MODULE__, {:append, event})
  end

  def subscribe(stream_name, batch_size) do
    GenServer.call(__MODULE__, {:subscribe, stream_name, batch_size})
  end

  def handle_call({:append, %Event{} = event}, _from, events) do
    GenServer.cast(self(), {:broadcast_event, event})
    {:reply, :ok, events ++ [event]}
  end

  def handle_call({:subscribe, stream_name, batch_size}, _from, events) do
    result = Subscription.subscribe_to(stream_name, events, batch_size)
    {:reply, result, events}
  end

  def handle_cast({:broadcast_event, event}, events) do
    Elescore.Supervisor.Store.Subscription
    |> DynamicSupervisor.which_children
    |> Enum.each(&notify_child(&1, event))

    {:noreply, events}
  end

  defp notify_child({:undefined, pid, :worker, [Elescore.Store.Subscription]}, event) do
    GenServer.call(pid, {:handle_event, event})
  end
  defp notify_child(_child, _event), do: nil

end
