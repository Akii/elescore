defmodule Elescore.Store.Subscription do
  @moduledoc """
  Represents a subscription to stream of events.
  """

  use GenServer
  alias Elescore.Store.{Event, Persistence}

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  def init(%{subscriber: subscriber} = state) do
    Process.link(subscriber)
    send(self(), :replay)
    {:ok, state}
  end

  def subscribe(stream_name, batch_size) do
    args = %{
      subscriber: self(),
      stream_name: stream_name,
      batch_size: batch_size,
      current_sequence: 0,
      mode: :replay
    }

    Supervisor.start_child(
      Elescore.Store.SubscriptionSupervisor,
      %{
        id: {__MODULE__, UUID.uuid4()},
        start: {__MODULE__, :start_link, [args]},
        restart: :temporary
      }
    )
  end

  def handle_info(:replay, state) do
    %{
      stream_name: stream_name,
      current_sequence: current_sequence,
      batch_size: batch_size,
      subscriber: subscriber
    } = state

    result = Persistence.read(stream_name, current_sequence, batch_size)

    case result do
      {:ok, events, next_sequence} ->
        new_state = %{state | current_sequence: next_sequence}
        :processed = GenServer.call(subscriber, {:next_events, events})
        send(self(), :replay)
        {:noreply, new_state}

      :end_of_stream ->
        {:noreply, %{state | mode: :continue}}
    end
  end

  def handle_cast({:handle_event, %Event{sequence: sequence} = event}, _from, state) do
    %{
      stream_names: stream_names,
      current_sequence: current_sequence,
      mode: mode,
      subscriber: subscriber
    } = state

    if is_subscribed_to_event(event, stream_names) do
      case mode do
        :replay ->
          {:noreply, state}

        :continue when sequence <= current_sequence ->
          {:noreply, state}

        :continue ->
          GenServer.call(subscriber, {:next_events, [event]})
          {:noreply, %{state | current_sequence: sequence}}
      end
    else
      {:noreply, state}
    end
  end

  defp is_subscribed_to_event(event, stream_names) do
    Enum.member?(stream_names, event.stream_name)
  end
end
