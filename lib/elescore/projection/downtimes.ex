defmodule Elescore.Projection.Downtimes do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_arg) do
    # We wait a bit for the disruption projection
    Process.send_after(self(), :calculate_downtimes, 30_000)
    {:ok, %{}}
  end

  def get_downtimes do
    GenServer.call(__MODULE__, :get_downtimes)
  end

  def handle_call(:get_downtimes, _from, state) do
    {:reply, state, state}
  end

  def handle_info(:calculate_downtimes, _state) do
    now = DateTime.utc_now()
    thirty_days_ago = DateTime.add(now, -thirty_days_in_seconds(), :second)
    matching_disruptions = get_unresolved_or_happend_within_given_time(thirty_days_ago)
    downtimes = Enum.reduce(matching_disruptions, %{}, &compute_downtimes_at_time(&1, &2, now))

    Process.send_after(self(), :calculate_downtimes, 60_000)
    {:noreply, downtimes}
  end

  defp get_unresolved_or_happend_within_given_time(date_time) do
    {:ok, res} = Sqlitex.Server.query(
      Elescore.Projection.ProjectionStore,
      "SELECT * FROM disruptions WHERE resolved_on IS NULL OR resolved_on >= ?1",
      bind: [DateTime.to_iso8601(date_time)],
      into: %{}
    )
    res
  end

  defp compute_downtimes_at_time(disruption_row, acc, now) do
    {:ok, resolved_on, _} =
      if disruption_row.resolved_on != nil do
        DateTime.from_iso8601(disruption_row.resolved_on)
      else
        {:ok, now, nil}
      end
    {:ok, occurred_on, _} = DateTime.from_iso8601(disruption_row.occurred_on)

    duration = DateTime.diff(resolved_on, occurred_on)

    Map.update(acc, disruption_row.facility_id, duration, &(&1 + duration))
  end

  defp thirty_days_in_seconds, do: 60 * 60 * 24 * 30
end
