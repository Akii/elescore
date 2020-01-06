defmodule Elescore.Projection.Disruptions do
  use GenServer
  use Elescore.Projection.SimpleProjection
  alias Elescore.Store.Event.{FacilityDisrupted, DisruptionReasonUpdated, FacilityRestored}

  defmodule State do
    defstruct active_disruptions: %{}, disruptions: %{}
  end

  defmodule Disruption do
    defstruct id: nil,
              facility_id: nil,
              occurred_on: nil,
              updated_on: nil,
              resolved_on: nil,
              reason: nil
  end

  def get_unresolved_or_happend_within_given_time(date_time) do
    GenServer.call(__MODULE__, {:get_unresolved_or_happend_within_given_time, date_time})
  end

  def stream_names, do: [:"Disruptions.DB"]

  def init_state, do: %State{}

  def apply_event(
        event,
        %State{active_disruptions: active_disruptions, disruptions: disruptions} = _state
      ) do
    facility_id = event.payload.facilityId
    disruption_id = Map.get(active_disruptions, facility_id, Enum.count(disruptions) + 1)
    disruption = make_disruption(disruption_id, event.occurred_on, event.payload)

    new_active_disruptions =
      if disruption.resolved_on != nil do
        Map.delete(active_disruptions, facility_id)
      else
        Map.put(active_disruptions, facility_id, disruption_id)
      end

    new_disruptions =
      Map.update(disruptions, disruption_id, disruption, &merge_disruption(disruption, &1))

    %State{active_disruptions: new_active_disruptions, disruptions: new_disruptions}
  end

  def handle_call({:next_events, events}, _from, state) do
    new_state = Enum.reduce(events, state, &apply_event/2)
    {:reply, :processed, new_state}
  end

  def handle_call(
        {:get_unresolved_or_happend_within_given_time, date_time},
        _from,
        %State{disruptions: disruptions} = state
      ) do
    matching_disruptions =
      disruptions
      |> Map.values()
      |> Enum.filter(&is_unresolved_or_happend_within_given_time(&1, date_time))

    {:reply, matching_disruptions, state}
  end

  defp make_disruption(id, occurred_on, %FacilityDisrupted{} = event) do
    %Disruption{
      id: id,
      facility_id: event.facilityId,
      occurred_on: occurred_on,
      updated_on: nil,
      resolved_on: nil,
      reason: event.reason
    }
  end

  defp make_disruption(id, occurred_on, %DisruptionReasonUpdated{} = event) do
    %Disruption{
      id: id,
      facility_id: event.facilityId,
      occurred_on: occurred_on,
      updated_on: occurred_on,
      resolved_on: nil,
      reason: event.reason
    }
  end

  defp make_disruption(id, occurred_on, %FacilityRestored{} = event) do
    %Disruption{
      id: id,
      facility_id: event.facilityId,
      occurred_on: occurred_on,
      updated_on: nil,
      resolved_on: occurred_on,
      reason: nil
    }
  end

  defp merge_disruption(%Disruption{} = d1, %Disruption{} = d2) do
    %Disruption{
      id: d1.id,
      facility_id: d1.facility_id,
      occurred_on: d2.occurred_on,
      updated_on: if(d1.updated_on != nil, do: d1.updated_on, else: d2.updated_on),
      resolved_on: if(d1.resolved_on != nil, do: d1.resolved_on, else: d2.resolved_on),
      reason: if(d1.reason != nil, do: d1.reason, else: d2.reason)
    }
  end

  defp is_unresolved_or_happend_within_given_time(
         %Disruption{resolved_on: resolved_on} = _disruption,
         date_time
       ) do
    resolved_on == nil || DateTime.compare(resolved_on, date_time) == :gt
  end
end
