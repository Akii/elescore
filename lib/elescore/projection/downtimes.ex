defmodule Elescore.Projection.Downtimes do
  use GenServer
  alias Elescore.Projection.Disruptions.Disruption

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_arg) do
    # We wait a bit for the disruption projection
    Process.send_after(self(), :calculate_downtimes, 15_000)
    downtimes_store = :ets.new(:projection_downtimes, [:named_table])
    {:ok, downtimes_store}
  end

  def handle_info(:calculate_downtimes, store) do
    now = DateTime.utc_now()
    thirty_days_ago = DateTime.add(now, -thirty_days_in_seconds(), :second)

    downtimes =
      Task.async(fn ->
        :ets.tab2list(:projection_disruptions)
        |> Stream.map(fn {_id, disruption} -> disruption end)
        |> Stream.filter(&is_unresolved_or_happend_within_given_time(&1, thirty_days_ago))
        |> Enum.reduce(%{}, &compute_downtimes_at_time(&1, &2, now))
      end)
      |> Task.await(:infinity)

    :ets.delete_all_objects(store)
    Enum.each(downtimes, &insert_downtime(store, &1))

    Process.send_after(self(), :calculate_downtimes, 60_000)
    {:noreply, store}
  end

  defp is_unresolved_or_happend_within_given_time(
         %Disruption{resolved_on: resolved_on} = _disruption,
         date_time
       ) do
    resolved_on == nil || DateTime.compare(resolved_on, date_time) == :gt
  end

  defp compute_downtimes_at_time(disruption, acc, now) do
    resolved_on = if(disruption.resolved_on != nil, do: disruption.resolved_on, else: now)
    duration = DateTime.diff(resolved_on, disruption.occurred_on)

    Map.update(
      acc,
      disruption.facility_id,
      duration,
      &min(duration + &1, thirty_days_in_seconds())
    )
  end

  defp insert_downtime(store, {facility_id, downtime}) do
    :ets.insert(store, {facility_id, downtime})
  end

  defp thirty_days_in_seconds, do: 60 * 60 * 24 * 30
end
