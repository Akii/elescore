defmodule Elescore.Projection.Downtimes do
  use GenServer
  alias Elescore.Projection.Disruptions

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_arg) do
    # We wait a bit for the disruption projection
    Process.send_after(self(), :calculate_downtimes, 30_000)
    {:ok, %{}}
  end

  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  def handle_info(:calculate_downtimes, _state) do
    now = DateTime.utc_now()
    thirty_days_ago = DateTime.add(now, -thirty_days_in_seconds(), :second)
    matching_disruptions = Disruptions.get_unresolved_or_happend_within_given_time(thirty_days_ago)
    downtimes = Enum.reduce(matching_disruptions, %{}, &compute_downtimes_at_time(&1, &2, now))

    Process.send_after(self(), :calculate_downtimes, 60_000)
    {:noreply, downtimes}
  end

  defp compute_downtimes_at_time(disruption, acc, now) do
    resolved_on = if(disruption.resolved_on != nil, do: disruption.resolved_on, else: now)
    duration = min(DateTime.diff(resolved_on, disruption.occurred_on), thirty_days_in_seconds())

    Map.update(acc, disruption.facility_id, duration, &(&1 + duration))
  end

  defp thirty_days_in_seconds, do: 60 * 60 * 24 * 30
end
