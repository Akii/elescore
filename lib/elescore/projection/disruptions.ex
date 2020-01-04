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

  def stream_name, do: :"Disruptions.DB"

  def init_state, do: %State{}

  def apply_event(event, state) do
    %State{active_disruptions: active_disruptions, disruptions: disruptions} = state

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
      occurred_on: d1.occurred_on,
      updated_on: if(d1.updated_on != nil, do: d1.updated_on, else: d2.updated_on),
      resolved_on: if(d1.resolved_on != nil, do: d1.resolved_on, else: d2.resolved_on),
      reason: if(d1.reason != nil, do: d1.reason, else: d2.reason)
    }
  end
end
