defmodule Elescore.Projection.Facilities do
  use Elescore.Projection.PersistedProjection

  alias Elescore.Store.Event
  alias Elescore.Store.Event.{
    FacilityDisrupted,
    FacilityRestored,
    FacilityIdentified,
    FacilityAssignedToObject,
    FacilityLocated,
    FacilityDescriptionUpdated,
    FacilityDeleted
  }

  defmodule Facility do
    @moduledoc false
    defstruct id: nil,
              objectId: nil,
              type: nil,
              name: nil,
              downtime: 0,
              isDisrupted: false,
              geoLocation: nil,
              deleted: false
  end

  def stream_names, do: [:"Disruptions.DB", :"Facilities.DB"]

  def table_name, do: "facilities"

  def table_schema,
    do: """
    id VARCHAR PRIMARY KEY,
    object_id VARCHAR,
    type VARCHAR,
    name VARCHAR,
    downtime INTEGER DEFAULT 0,
    is_disrupted BOOLEAN DEFAULT 0,
    geo_location_lat NUMERIC,
    geo_location_lng NUMERIC,
    deleted BOOLEAN DEFAULT 0
    """

  def init_state do
    send(self(), :update_downtime)
    nil
  end

  def apply_event(%Event{payload: payload} = _event, _state) do
    case payload do
      %FacilityDisrupted{facilityId: facility_id} ->
        update(facility_id, :is_disrupted, true)

      %FacilityRestored{facilityId: facility_id} ->
        update(facility_id, :is_disrupted, false)

      %FacilityIdentified{facilityId: facility_id, facilityType: facility_type, description: description} ->
        update(facility_id, :type, facility_type)
        update(facility_id, :name, description)
        update(facility_id, :deleted, false)

      %FacilityAssignedToObject{facilityId: facility_id, objectId: object_id} ->
        update(facility_id, :object_id, object_id)

      %FacilityLocated{facilityId: facility_id, geoLocation: geo_location} ->
        update(facility_id, :geo_location_lat, geo_location["lat"])
        update(facility_id, :geo_location_lng, geo_location["lng"])

      %FacilityDescriptionUpdated{facilityId: facility_id, description: description} ->
        update(facility_id, :name, description)

      %FacilityDeleted{facilityId: facility_id} ->
        update(facility_id, :deleted, true)

      _ -> nil
    end

    nil
  end

  def handle_info(:update_downtime, store) do
    downtimes = Elescore.Projection.Downtimes.get_downtimes()
    {:ok, facility_ids} = Sqlitex.Server.query(Elescore.Projection.ProjectionStore, "SELECT id FROM facilities")

    Enum.each(facility_ids, fn [id: facility_id] ->
      downtime_in_seconds = Map.get(downtimes, facility_id, 0)
      downtime_in_minutes = div(downtime_in_seconds, 60)
      downtime = min(downtime_in_minutes, thirty_days_in_minutes())

      update(facility_id, :downtime, downtime)
    end)

    Process.send_after(self(), :update_downtime, 60_000)
    {:noreply, store}
  end

  defp update(facility_id, field, value) do
    {:ok, _} = Sqlitex.Server.query(
      Elescore.Projection.ProjectionStore,
      """
      INSERT INTO facilities (id, #{field}) VALUES (?1, ?2)
      ON CONFLICT (id)
      DO UPDATE SET #{field} = excluded.#{field}
      """,
      bind: [facility_id, value]
    )
  end

  defp thirty_days_in_minutes(), do: 60 * 24 * 30
end
