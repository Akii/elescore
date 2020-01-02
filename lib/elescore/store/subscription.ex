defmodule Elescore.Store.Subscription do
  @moduledoc """
  Represents a subscription to stream of events.
  """

  use GenServer
  alias Elescore.Store.Event

  def child_spec(args) do
    %{
      id: {__MODULE__, UUID.uuid4()},
      start: {__MODULE__, :start_link, [args]},
      restart: :temporary
    }
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  def init(state) do
    relevant_events = state.events |> Enum.filter(&(&1.stream_name == state.stream_name))

    {:ok, Map.put(state, :events, relevant_events)}
  end

  def subscribe_to(stream_name, events, batch_size) do
    DynamicSupervisor.start_child(
      Elescore.Supervisor.Store.Subscription,
      {__MODULE__,
       %{
         stream_name: stream_name,
         events: events,
         batch_size: batch_size
       }}
    )
  end

  def get_next_events(subscription) do
    GenServer.call(subscription, :get_next_events)
  end

  def unsubscribe(subscription) do
    GenServer.stop(subscription)
  end

  def handle_call(:get_next_events, _from, state) do
    {next_events, remaining_events} = take_n_events(state.events, state.batch_size)
    new_state = Map.put(state, :events, remaining_events)
    {:reply, {:ok, next_events}, new_state}
  end

  def handle_call({:handle_event, %Event{} = event}, _from, state) do
    if event.stream_name == state.stream_name do
      added_event = state.events ++ [event]
      {:reply, :ok, Map.put(state, :events, added_event)}
    else
      {:reply, :ok, state}
    end
  end

  defp take_n_events(events, size) do
    {Enum.take(events, size), Enum.drop(events, size)}
  end
end
