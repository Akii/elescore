defmodule Elescore.Projection.DisruptionsPerDay do
  use Elescore.Projection.SimpleProjection
  alias Elescore.Store.Event.{FacilityDisrupted, FacilityRestored}
  import Statistics.IQR

  def stream_names, do: [:"Disruptions.DB"]

  def init_state, do: {%MapSet{}, %{}}

  def apply_event(event, state) do
    case event.payload do
      %FacilityDisrupted{facilityId: facility_id} ->
        add_sample(&MapSet.put/2, facility_id, event.occurred_on, state)

      %FacilityRestored{facilityId: facility_id} ->
        add_sample(&MapSet.delete/2, facility_id, event.occurred_on, state)

      _ ->
        state
    end
  end

  defp add_sample(f, facility_id, date_time, {disrupted_facilities, samples}) do
    new_disrupted_facilities = f.(disrupted_facilities, facility_id)
    number_of_disruptions = Enum.count(new_disrupted_facilities) / 1

    new_samples =
      Map.update(
        samples,
        to_date(date_time),
        singleton_sample(number_of_disruptions),
        &(&1 ++ singleton_sample(number_of_disruptions))
      )

    {new_disrupted_facilities, new_samples}
  end

  defp to_date(date_time) do
    date_time
    |> DateTime.to_iso8601()
    |> String.slice(0..9)
  end
end
