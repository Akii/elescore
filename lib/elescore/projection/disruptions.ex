defmodule Elescore.Projection.Disruptions do
  use Elescore.Projection.PersistedProjection
  alias Elescore.Store.Event.{FacilityDisrupted, DisruptionReasonUpdated, FacilityRestored}

  defmodule State do
    @moduledoc false
    defstruct active_disruptions: %{}, number_of_disruptions: 0
  end

  def stream_names, do: [:"Disruptions.DB"]

  def table_name, do: "disruptions"

  def table_schema,
    do: """
    id INTEGER PRIMARY KEY,
    facility_id VARCHAR NOT NULL,
    occurred_on VARCHAR NOT NULL,
    updated_on VARCHAR,
    resolved_on VARCHAR,
    reason VARCHAR
    """

  def init_state, do: %State{}

  def apply_event(
        event,
        %State{
          active_disruptions: active_disruptions,
          number_of_disruptions: number_of_disruptions
        } = state
      ) do
    facility_id = event.payload.facilityId
    disruption_id = Map.get(active_disruptions, facility_id, state.number_of_disruptions + 1)

    new_active_disruptions =
      case event.payload do
        %FacilityRestored{} -> Map.delete(active_disruptions, facility_id)
        _ -> Map.put(active_disruptions, facility_id, disruption_id)
      end

    case event.payload do
      %FacilityDisrupted{} = payload ->
        {:ok, []} =
          Sqlitex.Server.query(
            Elescore.Projection.ProjectionStore,
            "INSERT INTO disruptions (id, facility_id, occurred_on, reason) VALUES (?1, ?2, ?3, ?4)",
            bind: [
              disruption_id,
              payload.facilityId,
              DateTime.to_iso8601(event.occurred_on),
              payload.reason
            ]
          )

      %DisruptionReasonUpdated{} = payload ->
        {:ok, _} =
          Sqlitex.Server.query(
            Elescore.Projection.ProjectionStore,
            "UPDATE disruptions SET reason = ?1, updated_on = ?2 WHERE id = ?3",
            bind: [payload.reason, DateTime.to_iso8601(event.occurred_on), disruption_id]
          )

      %FacilityRestored{} = _payload ->
        {:ok, _} =
          Sqlitex.Server.query(
            Elescore.Projection.ProjectionStore,
            "UPDATE disruptions SET resolved_on = ?1 WHERE id = ?2",
            bind: [DateTime.to_iso8601(event.occurred_on), disruption_id]
          )

      _ ->
        nil
    end

    new_number_of_disruptions =
      if Enum.member?(active_disruptions, facility_id) do
        number_of_disruptions
      else
        number_of_disruptions + 1
      end

    %State{
      active_disruptions: new_active_disruptions,
      number_of_disruptions: new_number_of_disruptions
    }
  end
end
